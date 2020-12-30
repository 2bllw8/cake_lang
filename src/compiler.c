#include <memory.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "include/chunk.h"
#include "include/common.h"
#include "include/compiler.h"
#include "include/memory.h"
#include "include/object.h"
#include "include/scanner.h"
#include "include/token.h"
#include "include/value.h"

#ifdef DEBUG_PRINT_CODE
#include "include/debug.h"
#endif

/* **** Structures **** */

typedef struct {
	Token current;
	Token previous;
	bool hadError;
	bool panicMode;
} Parser;

typedef enum {
	PREC_NONE,
	PREC_ASSIGNMENT,
	PREC_OR,
	PREC_AND,
	PREC_EQUALITY,
	PREC_COMPARISON,
	PREC_TERM,
	PREC_FACTOR,
	PREC_UNARY,
	PREC_CALL,
	PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool can_assign);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

typedef struct {
	Token name;
	int depth;
	bool is_captured;
} Local;

typedef struct {
	uint8_t index;
	bool is_local;
} Upvalue;

typedef enum {
	TYPE_FUNCTION,
	TYPE_INITIALIZER,
	TYPE_METHOD,
	TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
	struct Compiler *enclosing;
	ObjFunction *function;
	FunctionType type;
	Local locals[UINT8_COUNT];
	int local_count;
	Upvalue upvalues[UINT8_COUNT];
	int scope_depth;
} Compiler;

typedef struct ClassCompiler {
	struct ClassCompiler *enclosing;
	Token name;
	bool has_super;
} ClassCompiler;

Parser parser;
Compiler *current = 0;
ClassCompiler *current_class = 0;

/* **** Utils **** */

static Chunk *currentChunk()
{
	return &current->function->chunk;
}

static void errorAt(Token *token, const char *message)
{
	if (parser.panicMode)
		return;
	parser.panicMode = true;

	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type != TOKEN_ERROR) {
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}
	fprintf(stderr, ": %s\n", message);
	parser.hadError = true;
}

static void error(const char *message)
{
	errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char *message)
{
	errorAt(&parser.current, message);
}

static void advance()
{
	parser.previous = parser.current;

	while (1) {
		parser.current = scanToken();
		if (parser.current.type != TOKEN_ERROR)
			break;

		errorAtCurrent(parser.current.start);
	}
}

static void consume(TokenType type, const char *message)
{
	if (parser.current.type != type) {
		errorAtCurrent(message);
		return;
	}
	advance();
}

static bool check(TokenType type)
{
	return parser.current.type == type;
}

static bool match(TokenType type)
{
	if (!check(type))
		return false;
	advance();
	return true;
}

static void emitByte(uint8_t byte)
{
	writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t a, uint8_t b)
{
	emitByte(a);
	emitByte(b);
}

static int emitJump(uint8_t instruction)
{
	emitByte(instruction);
	emitBytes(0xff, 0xff);
	return currentChunk()->count - 2;
}

static void emitLoop(int loop_start)
{
	emitByte(OP_LOOP);

	// +2 to take into account the size of the OP_LOOP instruction's operands
	int offset = currentChunk()->count - loop_start + 2;
	if (offset > UINT16_MAX)
		error("Loop body is too large");

	emitBytes((offset >> 8) & 0xff, offset & 0xff);
}

static void emitReturn()
{
	if (current->type == TYPE_INITIALIZER) {
		emitBytes(OP_GET_LOCAL, 0);
	} else {
		emitByte(OP_NIL);
	}
	emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value)
{
	int constant = addConstant(currentChunk(), value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk");
		return 0;
	}
	return (uint8_t)constant;
}

static void emitConstant(Value value)
{
	emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset)
{
	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk()->count - offset - 2;
	if (jump > UINT16_MAX) {
		error("Too much code to jump over");
	}
	currentChunk()->code[offset] = (jump >> 8) & 0xff;
	currentChunk()->code[offset + 1] = jump & 0xff;
}

void initCompiler(Compiler *compiler, FunctionType type)
{
	compiler->enclosing = current;
	compiler->function = 0;
	compiler->local_count = 0;
	compiler->scope_depth = 0;
	compiler->type = type;
	compiler->function = newFunction();
	current = compiler;

	if (type != TYPE_SCRIPT) {
		current->function->name =
			copyString(parser.previous.start, parser.previous.length);
	}

	Local *local = &current->locals[current->local_count++];
	local->depth = 0;
	local->is_captured = false;
	if (type == TYPE_FUNCTION) {
		local->name.start = "";
		local->name.length = 0;
	} else {
		local->name.start = "this";
		local->name.length = 4;
	}
}

static ObjFunction *endCompiler()
{
	emitReturn();
	ObjFunction *function = current->function;

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(),
						 function->name ? function->name->chars : "<script>");
	}
#endif

	current = current->enclosing;
	return function;
}

static void beginScope()
{
	current->scope_depth++;
}

static void endScope()
{
	current->scope_depth--;
	while (current->local_count > 0 &&
		   current->locals[current->local_count - 1].depth >
			   current->scope_depth) {
		if (current->locals[current->local_count - 1].is_captured) {
			emitByte(OP_CLOSE_UPVALUE);
		} else {
			emitByte(OP_POP);
		}
		current->local_count--;
	}
}

static void synchronize()
{
	parser.panicMode = false;

	while (parser.current.type != TOKEN_EOF) {
		if (parser.previous.type == TOKEN_SEMICOLON)
			return;

		switch (parser.current.type) {
		case TOKEN_CLASS:
		case TOKEN_FUN:
		case TOKEN_VAR:
		case TOKEN_IF:
		case TOKEN_WHILE:
		case TOKEN_PRINT:
		case TOKEN_RETURN:
			return;
		default:; // Keep going
		}

		advance();
	}
}

static void expression();
static void statement();
static void declaration();

static ParseRule *getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/* **** Variables **** */

static uint8_t identifierConstant(Token *name)
{
	return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b)
{
	return a->length == b->length && memcmp(a->start, b->start, a->length) == 0;
}

static void addLocal(Token name)
{
	if (current->local_count == UINT8_COUNT) {
		error("Reached limit of local variables in the current block");
		return;
	}

	Local *local = &current->locals[current->local_count++];
	local->name = name;
	local->depth = -1;
	local->is_captured = false;
}

static int resolveLocal(Compiler *compiler, Token *name)
{
	for (int i = compiler->local_count - 1; i >= 0; i--) {
		Local *local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1)
				error("Can't read local variable in its own initializer");
			return i;
		}
	}
	return -1;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool is_local)
{
	int upvalue_count = compiler->function->upvalue_count;

	for (int i = 0; i < upvalue_count; i++) {
		Upvalue *upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->is_local == is_local) {
			return i;
		}
	}

	compiler->upvalues[upvalue_count].is_local = is_local;
	compiler->upvalues[upvalue_count].index = index;
	return compiler->function->upvalue_count++;
}

static int resolveUpvalue(Compiler *compiler, Token *name)
{
	if (!compiler->enclosing)
		return -1;

	int local = resolveLocal(compiler->enclosing, name);
	if (local != -1) {
		compiler->enclosing->locals[local].is_captured = true;
		return addUpvalue(compiler, (uint8_t)local, true);
	}

	int upvalue = resolveUpvalue(compiler->enclosing, name);
	if (upvalue != -1) {
		return addUpvalue(compiler, (uint8_t)upvalue, false);
	}

	return -1;
}

static void markInitialized()
{
	if (current->scope_depth == 0)
		return;
	current->locals[current->local_count - 1].depth = current->scope_depth;
}

static void defineVariable(uint8_t global)
{
	if (current->scope_depth > 0) {
		markInitialized();
		return;
	}
	emitBytes(OP_DEFINE_GLOBAL, global);
}

static void declareVariable()
{
	if (current->scope_depth == 0)
		return;
	Token *name = &parser.previous;

	for (int i = current->local_count; i >= 0; i--) {
		Local *local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scope_depth) {
			break;
		}
		if (identifiersEqual(name, &local->name)) {
			error("Variable with the same name already exists in this scope");
		}
	}
	addLocal(*name);
}

static uint8_t parseVariable(const char *errorMessage)
{
	consume(TOKEN_IDENTIFIER, errorMessage);

	declareVariable();
	if (current->scope_depth > 0)
		return 0;
	return identifierConstant(&parser.previous);
}

static uint8_t argumentList()
{
	uint8_t argc = 0;
	if (!check(TOKEN_RIGHT_PARENTHESIS)) {
		do {
			expression();
			if (argc == 255) {
				error("Can't have more that 255 arguments");
			}
			argc++;
		} while (match(TOKEN_COMMA));
	}

	consume(TOKEN_RIGHT_PARENTHESIS, "Expected ')' after arguments");
	return argc;
}

static void namedVariable(Token name, bool can_assign)
{
	uint8_t get_op;
	uint8_t set_op;
	int arg = resolveLocal(current, &name);
	if (arg != -1) {
		get_op = OP_GET_LOCAL;
		set_op = OP_SET_LOCAL;
	} else if ((arg = resolveUpvalue(current, &name)) != -1) {
		get_op = OP_GET_UPVALUE;
		set_op = OP_SET_UPVALUE;
	} else {
		arg = identifierConstant(&name);
		get_op = OP_GET_GLOBAL;
		set_op = OP_SET_GLOBAL;
	}
	if (can_assign && match(TOKEN_EQUAL)) {
		expression();
		emitBytes(set_op, (uint8_t)arg);
	} else {
		emitBytes(get_op, (uint8_t)arg);
	}
}

static void variable(bool can_assign)
{
	namedVariable(parser.previous, can_assign);
}

static Token syntheticToken(const char *text)
{
	Token token;
	token.start = text;
	token.length = (int)strlen(text);
	return token;
}

static void thiz(bool can_assign)
{
	if (!current_class) {
		error("Can't use 'this' outside a class");
		return;
	}
	variable(false);
}

static void zuper(bool can_assign)
{
	if (!current_class) {
		error("Can't use 'super' outside of a class");
	} else if (!current_class->has_super) {
		error("Can't use 'super' in a class with no superclass");
	}

	consume(TOKEN_DOT, "Expected '.' after super");
	consume(TOKEN_IDENTIFIER, "Expected superclass method name after '.'");
	uint8_t name = identifierConstant(&parser.previous);

	namedVariable(syntheticToken("this"), false);
	if (match(TOKEN_LEFT_PARENTHESIS)) {
		uint8_t argc = argumentList();
		namedVariable(syntheticToken("super"), false);
		emitBytes(OP_SUPER_INVOKE, name);
		emitByte(argc);
	} else {
		namedVariable(syntheticToken("super"), false);
		emitBytes(OP_GET_SUPER, name);
	}
}

/* **** Statement **** */

/*
 * block -> "{" declaration* "}"
 */
static void block()
{
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
		declaration();
	}
	consume(TOKEN_RIGHT_BRACE, "Expected '}' after block");
}

/*
 * expressionStatement -> expression ";"
 */
static void expressionStatement()
{
	expression();
	consume(TOKEN_SEMICOLON, "Expected ';' after expression");
	emitByte(OP_POP);
}

/*
 * function -> ( parameters ) block
 */
static void function(FunctionType type)
{
	Compiler compiler;
	initCompiler(&compiler, type);
	// The compiler is ended when we reach the end of this scope, so
	// we won't need endScope() later
	beginScope();

	// Parameters
	consume(TOKEN_LEFT_PARENTHESIS, "Expected '(' after function name");
	if (!check(TOKEN_RIGHT_PARENTHESIS)) {
		do {
			current->function->arity++;
			if (current->function->arity > 255) {
				errorAtCurrent("Functions can't have more than 255 parameters");
			}
			uint8_t param_constant = parseVariable("Expected parameter name");
			defineVariable(param_constant);
		} while (match(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PARENTHESIS, "Expected ')' after parameters");

	// Body
	consume(TOKEN_LEFT_BRACE, "Expected '{' before function body");
	// block() already consumes TOKEN_RIGHT_BRACE
	block();

	ObjFunction *function = endCompiler();
	emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

	for (int i = 0; i < function->upvalue_count; i++) {
		emitByte(compiler.upvalues[i].is_local ? 1 : 0);
		emitByte(compiler.upvalues[i].index);
	}
}

/*
 * ifStatement -> "if" "(" expression ")" statement ("else" statement)?
 */
static void ifStatement()
{
	consume(TOKEN_LEFT_PARENTHESIS, "Expected '(' after if");
	expression();
	consume(TOKEN_RIGHT_PARENTHESIS, "Expected ')' after if condition");

	int then_jump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP);
	statement();

	int else_jump = emitJump(OP_JUMP);
	patchJump(then_jump);
	emitByte(OP_POP);

	if (match(TOKEN_ELSE))
		statement();
	patchJump(else_jump);
}

/*
 * method -> identifier(arguments?) { statement* }
 */
static void method()
{
	consume(TOKEN_IDENTIFIER, "Expected method name");
	uint8_t constant = identifierConstant(&parser.previous);

	FunctionType type = TYPE_METHOD;
	if (parser.previous.length == 4 &&
		memcmp(parser.previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}
	function(type);
	emitBytes(OP_METHOD, constant);
}

/*
 * printStatement -> "print" expression ";"
 */
static void printStatement()
{
	expression();
	consume(TOKEN_SEMICOLON, "Expected ';' after print value");
	emitByte(OP_PRINT);
}

/*
 * returnStatement -> "return" expression? ";"
 */
static void returnStatement()
{
	if (current->type == TYPE_SCRIPT) {
		// TODO: script exit values
		error("Can't return from top-level code");
	}

	if (match(TOKEN_SEMICOLON)) {
		emitReturn();
	} else {
		if (current->type == TYPE_INITIALIZER) {
			error("Can't return a value from an initializer");
		}

		expression();
		consume(TOKEN_SEMICOLON, "Expected ';' after return value");
		emitByte(OP_RETURN);
	}
}

/*
 * whileStatement -> while "(" expression ")" statement
 */
static void whileStatement()
{
	int loop_start = currentChunk()->count;

	consume(TOKEN_LEFT_PARENTHESIS, "Expected '(' after while");
	expression();
	consume(TOKEN_RIGHT_PARENTHESIS, "Expected ')' after while condition");

	int exit_jump = emitJump(OP_JUMP_IF_FALSE);

	emitByte(OP_POP);

	statement();
	emitLoop(loop_start);

	patchJump(exit_jump);
	emitByte(OP_POP);
}

/*
 * statement -> expressionStatement
 *            | printStatement
 *            | ifStatement
 *            | whileStatement
 *            | returnStatement
 *            | block
 */
static void statement()
{
	if (match(TOKEN_PRINT)) {
		printStatement();
	} else if (match(TOKEN_IF)) {
		ifStatement();
	} else if (match(TOKEN_WHILE)) {
		whileStatement();
	} else if (match(TOKEN_RETURN)) {
		returnStatement();
	} else if (match(TOKEN_LEFT_BRACE)) {
		beginScope();
		block();
		endScope();
	} else {
		expressionStatement();
	}
}

/* **** Declaration **** */

/*
 * classDeclaration -> class identifier (: identifier) block
 */
static void classDeclaration()
{
	consume(TOKEN_IDENTIFIER, "Expected class name");
	Token clazz_name = parser.previous;
	uint8_t name_constant = identifierConstant(&parser.previous);
	declareVariable();

	emitBytes(OP_CLASS, name_constant);
	defineVariable(name_constant);

	ClassCompiler class_compiler;
	class_compiler.name = parser.previous;
	class_compiler.enclosing = current_class;
	class_compiler.has_super = false;
	current_class = &class_compiler;

	if (match(TOKEN_COLON)) {
		consume(TOKEN_IDENTIFIER, "Expected superclass name");
		variable(false);
		if (identifiersEqual(&clazz_name, &parser.previous)) {
			error("A class can't inherit from itself");
		}

		beginScope();
		addLocal(syntheticToken("super"));
		defineVariable(0);
		namedVariable(clazz_name, false);
		emitByte(OP_INHERIT);
		class_compiler.has_super = true;
	}

	namedVariable(clazz_name, false);
	consume(TOKEN_LEFT_BRACE, "Expected '{' before class body");
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
		method();
	}
	consume(TOKEN_RIGHT_BRACE, "Expected '{' after class body");
	emitByte(OP_POP);

	if (class_compiler.has_super) {
		endScope();
	}
	current_class = current_class->enclosing;
}

/*
 * funDeclaration -> "fun" identifier function
 */
static void funDeclaration()
{
	uint8_t global = parseVariable("Expected function name");
	markInitialized();
	function(TYPE_FUNCTION);
	defineVariable(global);
}

/*
 * varDeclaration -> "var" identifier (= expression)? ";"
 */
static void varDeclaration()
{
	uint8_t global = parseVariable("Expected variable name");
	if (match(TOKEN_EQUAL)) {
		expression();
	} else {
		emitByte(OP_NIL);
	}
	consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration");
	defineVariable(global);
}

/*
 * declaration -> classDeclaration
 *              | funDeclaration
 *              | varDeclaration
 *              | statement
 */
static void declaration()
{
	if (match(TOKEN_CLASS)) {
		classDeclaration();
	} else if (match(TOKEN_FUN)) {
		funDeclaration();
	} else if (match(TOKEN_VAR)) {
		varDeclaration();
	} else {
		statement();
	}

	if (parser.panicMode)
		synchronize();
}

/* **** Expressions **** */

static void expression()
{
	parsePrecedence(PREC_ASSIGNMENT);
}

static void andOp(bool can_assign)
{
	int end_jump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP);
	parsePrecedence(PREC_AND);
	patchJump(end_jump);
}

static void orOp(bool can_assign)
{
	int else_jump = emitJump(OP_JUMP_IF_FALSE);
	int end_jump = emitJump(OP_JUMP);

	patchJump(else_jump);
	emitByte(OP_POP);
	parsePrecedence(PREC_OR);
	patchJump(end_jump);
}

static void binary(bool can_assign)
{
	TokenType operator_type = parser.previous.type;

	ParseRule *rule = getRule(operator_type);
	parsePrecedence((Precedence)(rule->precedence + 1));

	switch (operator_type) {
	case TOKEN_BANG_EQUAL:
		emitBytes(OP_EQUAL, OP_NOT);
		break;
	case TOKEN_EQUAL_EQUAL:
		emitByte(OP_EQUAL);
		break;
	case TOKEN_GREATER:
		emitByte(OP_GREATER);
		break;
	case TOKEN_GREATER_EQUAL:
		emitBytes(OP_LESS, OP_NOT);
		break;
	case TOKEN_LESS:
		emitByte(OP_LESS);
		break;
	case TOKEN_LESS_EQUAL:
		emitBytes(OP_GREATER, OP_NOT);
		break;
	case TOKEN_PLUS:
		emitByte(OP_ADD);
		break;
	case TOKEN_MINUS:
		emitByte(OP_SUBTRACT);
		break;
	case TOKEN_STAR:
		emitByte(OP_MULTIPLY);
		break;
	case TOKEN_SLASH:
		emitByte(OP_DIVIDE);
		break;
	default:
		return;
	}
}

static void call(bool can_assign)
{
	uint8_t argc = argumentList();
	emitBytes(OP_CALL, argc);
}

static void dot(bool can_assign)
{
	consume(TOKEN_IDENTIFIER, "Expected property name after '.'");
	uint8_t name = identifierConstant(&parser.previous);

	if (can_assign && match(TOKEN_EQUAL)) {
		expression();
		emitBytes(OP_SET_PROPERTY, name);
	} else if (match(TOKEN_LEFT_PARENTHESIS)) {
		uint8_t argc = argumentList();
		emitBytes(OP_INVOKE, name);
		emitByte(argc);
	} else {
		emitBytes(OP_GET_PROPERTY, name);
	}
}

static void grouping(bool can_assign)
{
	expression();
	consume(TOKEN_RIGHT_PARENTHESIS, "Expected ')' after expression");
}

static void literal(bool can_assign)
{
	switch (parser.previous.type) {
	case TOKEN_FALSE:
		emitByte(OP_FALSE);
		break;
	case TOKEN_NIL:
		emitByte(OP_NIL);
		break;
	case TOKEN_TRUE:
		emitByte(OP_TRUE);
		break;
	default:
		return;
	}
}

static void floatNumber(bool can_assign)
{
	double value = strtod(parser.previous.start, 0);
	emitConstant(FLOAT_VAL(value));
}

static void intNumber(bool can_assign)
{
	long value = strtol(parser.previous.start, 0, 10);
	emitConstant(INT_VAL(value));
}

static void string(bool can_assign)
{
	emitConstant(OBJ_VAL(
		copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void unary(bool can_assign)
{
	TokenType operator_type = parser.previous.type;

	// Compile the operand
	parsePrecedence(PREC_UNARY);

	switch (operator_type) {
	case TOKEN_BANG:
		emitByte(OP_NOT);
		break;
	case TOKEN_MINUS:
		emitByte(OP_NEGATE);
		break;
	case TOKEN_TO_FLOAT:
		emitByte(OP_TO_FLOAT);
		break;
	case TOKEN_TO_INT:
		emitByte(OP_TO_INT);
		break;
	default:
		return;
	}
}

/* **** Rules **** */

ParseRule rules[] = {
	[TOKEN_AND] = { 0, andOp, PREC_AND },
	[TOKEN_BANG] = { unary, 0, PREC_NONE },
	[TOKEN_BANG_EQUAL] = { 0, binary, PREC_EQUALITY },
	[TOKEN_CLASS] = { 0, 0, PREC_NONE },
	[TOKEN_COMMA] = { 0, 0, PREC_NONE },
	[TOKEN_DOT] = { 0, dot, PREC_CALL },
	[TOKEN_ELSE] = { 0, 0, PREC_NONE },
	[TOKEN_EOF] = { 0, 0, PREC_NONE },
	[TOKEN_EQUAL] = { 0, 0, PREC_NONE },
	[TOKEN_EQUAL_EQUAL] = { 0, binary, PREC_EQUALITY },
	[TOKEN_ERROR] = { 0, 0, PREC_NONE },
	[TOKEN_FALSE] = { literal, 0, PREC_NONE },
	[TOKEN_FLOAT] = { floatNumber, 0, PREC_NONE },
	[TOKEN_FUN] = { 0, 0, PREC_NONE },
	[TOKEN_GREATER] = { 0, binary, PREC_COMPARISON },
	[TOKEN_GREATER_EQUAL] = { 0, binary, PREC_COMPARISON },
	[TOKEN_IDENTIFIER] = { variable, 0, PREC_NONE },
	[TOKEN_IF] = { 0, 0, PREC_NONE },
	[TOKEN_INT] = { intNumber, 0, PREC_NONE },
	[TOKEN_LEFT_BRACE] = { 0, 0, PREC_NONE },
	[TOKEN_LEFT_PARENTHESIS] = { grouping, call, PREC_CALL },
	[TOKEN_LESS] = { 0, binary, PREC_COMPARISON },
	[TOKEN_LESS_EQUAL] = { 0, binary, PREC_COMPARISON },
	[TOKEN_MINUS] = { unary, binary, PREC_TERM },
	[TOKEN_NIL] = { literal, 0, PREC_NONE },
	[TOKEN_OR] = { 0, orOp, PREC_OR },
	[TOKEN_PLUS] = { 0, binary, PREC_TERM },
	[TOKEN_PRINT] = { 0, 0, PREC_NONE },
	[TOKEN_RETURN] = { 0, 0, PREC_NONE },
	[TOKEN_RIGHT_BRACE] = { 0, 0, PREC_NONE },
	[TOKEN_RIGHT_PARENTHESIS] = { 0, 0, PREC_NONE },
	[TOKEN_SEMICOLON] = { 0, 0, PREC_NONE },
	[TOKEN_SLASH] = { 0, binary, PREC_FACTOR },
	[TOKEN_STAR] = { 0, binary, PREC_FACTOR },
	[TOKEN_STRING] = { string, 0, PREC_NONE },
	[TOKEN_SUPER] = { zuper, 0, PREC_NONE },
	[TOKEN_THIS] = { thiz, 0, PREC_NONE },
	[TOKEN_TO_FLOAT] = { unary, 0, PREC_UNARY },
	[TOKEN_TO_INT] = { unary, 0, PREC_UNARY },
	[TOKEN_TRUE] = { literal, 0, PREC_NONE },
	[TOKEN_VAR] = { 0, 0, PREC_NONE },
	[TOKEN_WHILE] = { 0, 0, PREC_NONE },
};

static ParseRule *getRule(TokenType type)
{
	return &rules[type];
}

static void parsePrecedence(Precedence precedence)
{
	advance();
	ParseFn prefix_rule = getRule(parser.previous.type)->prefix;
	if (!prefix_rule) {
		error("Expected expression");
		return;
	}

	bool can_assign = precedence <= PREC_ASSIGNMENT;
	prefix_rule(can_assign);

	while (precedence <= getRule(parser.current.type)->precedence) {
		advance();
		ParseFn infix_rule = getRule(parser.previous.type)->infix;
		infix_rule(can_assign);
	}

	if (can_assign && match(TOKEN_EQUAL)) {
		error("Invalid assignment target");
	}
}

ObjFunction *compile(const char *source)
{
	initScanner(source);
	Compiler compiler;
	initCompiler(&compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;

	advance();
	while (!match(TOKEN_EOF)) {
		declaration();
	}

	ObjFunction *function = endCompiler();
	return parser.hadError ? 0 : function;
}

void markCompilerRoots()
{
	Compiler *compiler = current;
	while (compiler) {
		markObject((Obj *)compiler->function);
		compiler = compiler->enclosing;
	}
}