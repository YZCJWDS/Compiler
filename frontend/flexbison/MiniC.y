%{
#include <cstdio>
#include <cstring>

// 词法分析头文件
#include "FlexLexer.h"

// bison生成的头文件
#include "BisonParser.h"

// 抽象语法树函数定义原型头文件
#include "AST.h"

#include "IntegerType.h"
#include "VoidType.h"
#include "PointerType.h"

// LR分析失败时所调用函数的原型声明
void yyerror(char * msg);

%}

// 联合体声明，用于后续终结符和非终结符号属性指定使用
%union {
    class ast_node * node;

    struct digit_int_attr integer_num;
    struct digit_real_attr float_num;
    struct var_id_attr var_id;
    struct type_attr type;
    int op_class;
};

// 文法的开始符号
%start  CompileUnit

// 指定文法的终结符号，<>可指定文法属性
// 对于单个字符的算符或者分隔符，在词法分析时可直返返回对应的ASCII码值，bison预留了255以内的值
// %token开始的符号称之为终结符，需要词法分析工具如flex识别后返回
// %type开始的符号称之为非终结符，需要通过文法产生式来定义
// %token或%type之后的<>括住的内容成为文法符号的属性，定义在前面的%union中的成员名字。
%token <integer_num> T_DIGIT
%token <var_id> T_ID
%token <type> T_INT T_VOID

// 关键或保留字 一词一类 不需要赋予语义属性
%token T_RETURN T_IF T_ELSE T_WHILE T_FOR T_BREAK T_CONTINUE
// 新增关系运算符token
%token T_GE T_GT T_LE T_LT T_EQ T_NE
// 新增逻辑运算符token
%token T_AND T_OR T_NOT

// 分隔符 一词一类 不需要赋予语义属性
%token T_SEMICOLON T_L_PAREN T_R_PAREN T_L_BRACE T_R_BRACE T_L_BRACKET T_R_BRACKET
%token T_COMMA

// 运算符
%token T_ASSIGN T_SUB T_ADD T_MUL T_DIV T_MOD

// 非终结符类型声明
%type <node> CompileUnit FuncDef Block BlockItemList BlockItem Statement Expr LVal
%type <node> VarDecl VarDeclExpr VarDef AddExp UnaryExp PrimaryExp MulExp RealParamList
%type <type> BasicType
%type <op_class> AddOp MulOp UnaryOp
%type <node> LogicalOrExp LogicalAndExp RelExp  // 新增RelExp语义类型
%type <node> FormalParams FormalParamList FormalParam // 新增形参相关非终结符

%left T_OR
%left T_AND
%left T_EQ T_NE
%left T_GT T_LT T_GE T_LE
%left T_ADD T_SUB
%left T_MUL T_DIV T_MOD

%%


// 编译单元可包含若干个函数与全局变量定义。要在语义分析时检查main函数存在
// compileUnit: (funcDef | varDecl)* EOF;
// bison不支持闭包运算，为便于追加修改成左递归方式
// compileUnit: funcDef | varDecl | compileUnit funcDef | compileUnit varDecl
CompileUnit : FuncDef {

		// 创建一个编译单元的节点AST_OP_COMPILE_UNIT
		$$ = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT, $1);

		// 设置到全局变量中
		ast_root = $$;
	}
	| VarDecl {

		// 创建一个编译单元的节点AST_OP_COMPILE_UNIT
		$$ = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT, $1);
		ast_root = $$;
	}
	| CompileUnit FuncDef {

		// 把函数定义的节点作为编译单元的孩子
		$$ = $1->insert_son_node($2);
	}
	| CompileUnit VarDecl {
		// 把变量定义的节点作为编译单元的孩子
		$$ = $1->insert_son_node($2);
	}
	;

// 函数定义，支持整数或void返回类型，支持形参
FuncDef : BasicType T_ID T_L_PAREN T_R_PAREN Block  {
		// 函数返回类型
		type_attr funcReturnType = $1;

		// 函数名
		var_id_attr funcId = $2;

		// 函数体节点即Block，即$5
		ast_node * blockNode = $5;

		// 形参结点没有，设置为空指针
		ast_node * formalParamsNode = nullptr;

		// 创建函数定义的节点，孩子有类型，函数名，语句块和形参(实际上无)
		// create_func_def函数内会释放funcId中指向的标识符空间，切记，之后不要再释放，之前一定要是通过strdup函数或者malloc分配的空间
		$$ = create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
	}
	| BasicType T_ID T_L_PAREN FormalParams T_R_PAREN Block {
		// 函数返回类型
		type_attr funcReturnType = $1;

		// 函数名
		var_id_attr funcId = $2;

		// 形参节点
		ast_node * formalParamsNode = $4;

		// 函数体节点即Block
		ast_node * blockNode = $6;

		// 创建函数定义的节点，孩子有类型，函数名，形参和语句块
		$$ = create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
	}
	;

// 形参列表，可以为空
FormalParams : {
		// 空的形参列表
		$$ = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS);
	}
	| FormalParamList {
		// 非空形参列表
		$$ = $1;
	}
	;

// 形参列表，至少有一个形参
FormalParamList : FormalParam {
		// 创建形参列表节点
		$$ = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS, $1);
	}
	| FormalParamList T_COMMA FormalParam {
		// 添加形参到列表
		$$ = $1->insert_son_node($3);
	}
	;

// 单个形参
FormalParam : BasicType T_ID {
		// 普通变量形参，例如: int a
		
		// 创建类型节点
		ast_node * type_node = create_type_node($1);
		
		// 创建变量ID节点
		ast_node * id_node = ast_node::New($2.id, $2.lineno);
		
		// 释放字符串空间
		free($2.id);
		
		// 创建形参节点，包含类型和ID
		$$ = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAM, type_node, id_node);
	}
	| BasicType T_ID T_L_BRACKET T_R_BRACKET {
		// 数组形参，例如: int a[]
		
		// 创建类型节点（保持为基本类型）
		ast_node * type_node = create_type_node($1);
		
		// 创建变量ID节点
		ast_node * id_node = ast_node::New($2.id, $2.lineno);
		
		// 创建数组标记节点（用一个整数0表示数组参数）
		ast_node * array_marker = ast_node::New(digit_int_attr{0, $2.lineno});
		
		// 释放字符串空间
		free($2.id);
		
		// 创建形参节点，包含类型、ID和数组标记
		$$ = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAM, type_node, id_node, array_marker);
	}
	;

// 语句块的文法Block ： T_L_BRACE BlockItemList? T_R_BRACE
// 其中?代表可有可无，在bison中不支持，需要拆分成两个产生式
// Block ： T_L_BRACE T_R_BRACE | T_L_BRACE BlockItemList T_R_BRACE
Block : T_L_BRACE T_R_BRACE {
		// 语句块没有语句

		// 为了方便创建一个空的Block节点
		$$ = create_contain_node(ast_operator_type::AST_OP_BLOCK);
	}
	| T_L_BRACE BlockItemList T_R_BRACE {
		// 语句块含有语句

		// BlockItemList归约时内部创建Block节点，并把语句加入，这里不创建Block节点
		$$ = $2;
	}
	;

// 语句块内语句列表的文法：BlockItemList : BlockItem+
// Bison不支持正闭包，需修改成左递归形式，便于属性的传递与孩子节点的追加
// 左递归形式的文法为：BlockItemList : BlockItem | BlockItemList BlockItem
BlockItemList : BlockItem {
		// 第一个左侧的孩子节点归约成Block节点，后续语句可持续作为孩子追加到Block节点中
		// 创建一个AST_OP_BLOCK类型的中间节点，孩子为Statement($1)
		$$ = create_contain_node(ast_operator_type::AST_OP_BLOCK, $1);
	}
	| BlockItemList BlockItem {
		// 把BlockItem归约的节点加入到BlockItemList的节点中
		$$ = $1->insert_son_node($2);
	}
	;


// 语句块中子项的文法：BlockItem : Statement
// 目前只支持语句,后续可增加支持变量定义
BlockItem : Statement  {
		// 语句节点传递给归约后的节点上，综合属性
		$$ = $1;
	}
	| VarDecl {
		// 变量声明节点传递给归约后的节点上，综合属性
		$$ = $1;
	}
	;

// 变量声明语句
// 语法：varDecl: basicType varDef (T_COMMA varDef)* T_SEMICOLON
// 因Bison不支持闭包运算符，因此需要修改成左递归，修改后的文法为：
// VarDecl : VarDeclExpr T_SEMICOLON
// VarDeclExpr: BasicType VarDef | VarDeclExpr T_COMMA varDef
VarDecl : VarDeclExpr T_SEMICOLON {
		$$ = $1;
	}
	;

// 变量声明表达式，可支持逗号分隔定义多个
VarDeclExpr: BasicType VarDef {

		// 创建类型节点
		ast_node * type_node = create_type_node($1);

		// 创建变量定义节点
		ast_node * decl_node = create_contain_node(ast_operator_type::AST_OP_VAR_DECL, type_node, $2);
		decl_node->type = type_node->type;

		// 创建变量声明语句，并加入第一个变量
		$$ = create_var_decl_stmt_node(decl_node);
	}
	| VarDeclExpr T_COMMA VarDef {

		// 创建类型节点，这里从VarDeclExpr获取类型，前面已经设置
		ast_node * type_node = ast_node::New($1->type);

		// 创建变量定义节点
		ast_node * decl_node = create_contain_node(ast_operator_type::AST_OP_VAR_DECL, type_node, $3);

		// 插入到变量声明语句
		$$ = $1->insert_son_node(decl_node);
	}
	;

// 变量定义包含变量名，实际上还有初值，这里没有实现。
VarDef : T_ID {
		// 变量ID

		$$ = ast_node::New(var_id_attr{$1.id, $1.lineno});

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);
	}
    | T_ID T_L_BRACKET T_DIGIT T_R_BRACKET {
        // 一维数组变量定义，例如: int a[10];
        
        // 创建变量ID节点
        ast_node* id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});
        
        // 创建数组大小节点
        ast_node* size_node = ast_node::New($3);
        
        // 释放字符串空间
        free($1.id);
        
        // 创建数组定义节点
        $$ = create_contain_node(ast_operator_type::AST_OP_ARRAY_DEF, id_node, size_node);
    }
    | T_ID T_L_BRACKET T_DIGIT T_R_BRACKET T_L_BRACKET T_DIGIT T_R_BRACKET {
        // 二维数组变量定义，例如: int a[10][20];
        
        // 创建变量ID节点
        ast_node* id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});
        
        // 创建第一维大小节点
        ast_node* size1_node = ast_node::New($3);
        
        // 创建第二维大小节点
        ast_node* size2_node = ast_node::New($6);
        
        // 释放字符串空间
        free($1.id);
        
        // 创建二维数组定义节点
        $$ = create_contain_node(ast_operator_type::AST_OP_ARRAY_DEF, id_node, size1_node, size2_node);
    }
    | T_ID T_L_BRACKET T_DIGIT T_R_BRACKET T_L_BRACKET T_DIGIT T_R_BRACKET T_L_BRACKET T_DIGIT T_R_BRACKET {
        // 三维数组变量定义，例如: int a[4][2][1];
        
        // 创建变量ID节点
        ast_node* id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});
        
        // 创建第一维大小节点
        ast_node* size1_node = ast_node::New($3);
        
        // 创建第二维大小节点
        ast_node* size2_node = ast_node::New($6);
        
        // 创建第三维大小节点
        ast_node* size3_node = ast_node::New($9);
        
        // 释放字符串空间
        free($1.id);
        
        // 创建三维数组定义节点
        $$ = create_contain_node(ast_operator_type::AST_OP_ARRAY_DEF, id_node, size1_node, size2_node, size3_node);
    }
    | T_ID T_ASSIGN Expr {
        // 变量ID及初始值

        // 创建变量ID节点
        ast_node* id_node = ast_node::New(var_id_attr{$1.id, $1.lineno});
        
        // 释放字符串空间
        free($1.id);
        
        // 创建赋值节点，左边是ID，右边是表达式
        $$ = create_contain_node(ast_operator_type::AST_OP_ASSIGN, id_node, $3);
	}
	;

// 基本类型，支持整型和void类型
BasicType: T_INT {
		$$ = $1;
	}
	| T_VOID {
		$$ = $1;
	}
	;

// 语句文法：statement:T_RETURN expr T_SEMICOLON | lVal T_ASSIGN expr T_SEMICOLON
// | block | expr? T_SEMICOLON
// 支持返回语句、赋值语句、语句块、表达式语句
// 其中表达式语句可支持空语句，由于bison不支持?，修改成两条
// 修正后的Statement规则
Statement : 
    T_RETURN Expr T_SEMICOLON {
        $$ = create_contain_node(ast_operator_type::AST_OP_RETURN, $2);
    }
    | T_RETURN T_SEMICOLON {
        // 添加空返回语句支持
        $$ = create_contain_node(ast_operator_type::AST_OP_RETURN);
    }
    | LVal T_ASSIGN Expr T_SEMICOLON {
        $$ = create_contain_node(ast_operator_type::AST_OP_ASSIGN, $1, $3);
    }
    | Block {
        $$ = $1;
    }
    | Expr T_SEMICOLON {
        $$ = $1;
    }
    | T_SEMICOLON {
        $$ = nullptr;
    }
    // 新增控制结构
    | T_IF T_L_PAREN Expr T_R_PAREN Statement {
        $$ = create_contain_node(ast_operator_type::AST_OP_CONDITIONAL, $3, $5);
    }
    | T_IF T_L_PAREN Expr T_R_PAREN Statement T_ELSE Statement {
        $$ = create_contain_node(ast_operator_type::AST_OP_CONDITIONAL, $3, $5, $7);
    }
    | T_WHILE T_L_PAREN Expr T_R_PAREN Statement {
        $$ = create_contain_node(ast_operator_type::AST_OP_LOOP, $3, $5);
    }
    | T_FOR T_L_PAREN Expr T_SEMICOLON Expr T_SEMICOLON Expr T_R_PAREN Statement {
        $$ = create_contain_node(ast_operator_type::AST_OP_FOR, $3, $5, $7, $9);
    }
    | T_BREAK T_SEMICOLON {
        $$ = create_contain_node(ast_operator_type::AST_OP_BREAK);
    }
    | T_CONTINUE T_SEMICOLON {
        $$ = create_contain_node(ast_operator_type::AST_OP_CONTINUE);
    }
    ;

// 表达式文法 expr : AddExp
// 表达式目前只支持加法与减法运算
Expr : LogicalOrExp {
        $$ = $1;
    }
    ;

LogicalOrExp : LogicalAndExp {
        $$ = $1;
    }
    | LogicalOrExp T_OR LogicalAndExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_LOGICAL_OR, $1, $3);
    }
    ;

LogicalAndExp : RelExp {
        $$ = $1;
    }
    | LogicalAndExp T_AND RelExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_LOGICAL_AND, $1, $3);
    }
    ;

RelExp : AddExp {
        $$ = $1;
    }
    | RelExp T_GT AddExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_GT, $1, $3);
    }
    | RelExp T_LT AddExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_LT, $1, $3);
    }
    | RelExp T_GE AddExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_GE, $1, $3);
    }
    | RelExp T_LE AddExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_LE, $1, $3);
    }
    | RelExp T_EQ AddExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_EQ, $1, $3);
    }
    | RelExp T_NE AddExp {
        $$ = create_contain_node(ast_operator_type::AST_OP_NE, $1, $3);
    }
    ;

// 加减表达式文法：addExp: unaryExp (addOp unaryExp)*
// 由于bison不支持用闭包表达，因此需要拆分成左递归的形式
// 改造后的左递归文法：
// addExp : unaryExp | unaryExp addOp unaryExp | addExp addOp unaryExp

AddExp : MulExp { $$ = $1; }
    | AddExp AddOp MulExp {
        $$ = create_contain_node(ast_operator_type($2), $1, $3);
    }
    ;
MulExp : UnaryExp { $$ = $1; }
    | MulExp MulOp UnaryExp { $$ = create_contain_node(ast_operator_type($2), $1, $3); }
    ;
// 加减运算符
AddOp: T_ADD {
		$$ = (int)ast_operator_type::AST_OP_ADD;
	}
	| T_SUB {
		$$ = (int)ast_operator_type::AST_OP_SUB;
	}
	;
MulOp: T_MUL { $$ = (int)ast_operator_type::AST_OP_MUL; }
    | T_DIV { $$ = (int)ast_operator_type::AST_OP_DIV; }
    | T_MOD { $$ = (int)ast_operator_type::AST_OP_MOD; }
    ;

UnaryOp: T_SUB { $$ = (int)ast_operator_type::AST_OP_NEG; }
    | T_NOT { $$ = (int)ast_operator_type::AST_OP_NOT; }
    ;
// 目前一元表达式可以为基本表达式、函数调用，其中函数调用的实参可有可无
// 其文法为：unaryExp: primaryExp | T_ID T_L_PAREN realParamList? T_R_PAREN
// 由于bison不支持？表达，因此变更后的文法为：
// unaryExp: primaryExp | T_ID T_L_PAREN T_R_PAREN | T_ID T_L_PAREN realParamList T_R_PAREN
UnaryExp : PrimaryExp {
		// 基本表达式

		// 传递到归约后的UnaryExp上
		$$ = $1;
	}
	| T_ID T_L_PAREN T_R_PAREN {
		// 没有实参的函数调用

		// 创建函数调用名终结符节点
		ast_node * name_node = ast_node::New(std::string($1.id), $1.lineno);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);

		// 实参列表
		ast_node * paramListNode = nullptr;

		// 创建函数调用节点，其孩子为被调用函数名和实参，实参为空，但函数内部会创建实参列表节点，无孩子
		$$ = create_func_call(name_node, paramListNode);

	}
	| T_ID T_L_PAREN RealParamList T_R_PAREN {
		// 含有实参的函数调用

		// 创建函数调用名终结符节点
		ast_node * name_node = ast_node::New(std::string($1.id), $1.lineno);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);

		// 实参列表
		ast_node * paramListNode = $3;

		// 创建函数调用节点，其孩子为被调用函数名和实参，实参不为空
		$$ = create_func_call(name_node, paramListNode);
	}
	;

// 基本表达式支持无符号整型字面量、带括号的表达式、具有左值属性的表达式
// 其文法为：primaryExp: T_L_PAREN expr T_R_PAREN | T_DIGIT | lVal
PrimaryExp :  T_L_PAREN Expr T_R_PAREN {
		// 带有括号的表达式
		$$ = $2;
	}
	| T_DIGIT {
        	// 无符号整型字面量

		// 创建一个无符号整型的终结符节点
		$$ = ast_node::New($1);
	}
	| LVal  {
		// 具有左值的表达式

		// 直接传递到归约后的非终结符号PrimaryExp
		$$ = $1;
	}
	  | UnaryOp UnaryExp {
        $$ = create_contain_node(ast_operator_type($1), $2);
    }
	;

// 实参表达式支持逗号分隔的若干个表达式
// 其文法为：realParamList: expr (T_COMMA expr)*
// 由于Bison不支持闭包运算符表达，修改成左递归形式的文法
// 左递归文法为：RealParamList : Expr | 左递归文法为：RealParamList T_COMMA expr
RealParamList : Expr {
		// 创建实参列表节点，并把当前的Expr节点加入
		$$ = create_contain_node(ast_operator_type::AST_OP_FUNC_REAL_PARAMS, $1);
	}
	| RealParamList T_COMMA Expr {
		// 左递归增加实参表达式
		$$ = $1->insert_son_node($3);
	}
	;

// 左值表达式，目前只支持变量名，实际上还有下标变量
LVal : T_ID {
		// 变量名终结符

		// 创建变量名终结符节点
		$$ = ast_node::New($1);

		// 对于字符型字面量的字符串空间需要释放，因词法用到了strdup进行了字符串复制
		free($1.id);
	}
    | T_ID T_L_BRACKET Expr T_R_BRACKET {
        // 数组元素访问，例如: a[i]
        
        // 创建变量名节点
        ast_node* id_node = ast_node::New($1);
        
        // 释放字符串空间
        free($1.id);
        
        // 创建数组访问节点
        $$ = create_contain_node(ast_operator_type::AST_OP_ARRAY_ACCESS, id_node, $3);
    }
    | T_ID T_L_BRACKET Expr T_R_BRACKET T_L_BRACKET Expr T_R_BRACKET {
        // 二维数组元素访问，例如: a[i][j]
        
        // 创建变量名节点
        ast_node* id_node = ast_node::New($1);
        
        // 释放字符串空间
        free($1.id);
        
        // 创建二维数组访问节点
        $$ = create_contain_node(ast_operator_type::AST_OP_ARRAY_ACCESS, id_node, $3, $6);
    }
    | T_ID T_L_BRACKET Expr T_R_BRACKET T_L_BRACKET Expr T_R_BRACKET T_L_BRACKET Expr T_R_BRACKET {
        // 三维数组元素访问，例如: a[i][j][k]
        
        // 创建变量名节点
        ast_node* id_node = ast_node::New($1);
        
        // 释放字符串空间
        free($1.id);
        
        // 创建三维数组访问节点
        $$ = create_contain_node(ast_operator_type::AST_OP_ARRAY_ACCESS, id_node, $3, $6, $9);
    }
	;

%%


// 语法识别错误要调用函数的定义
void yyerror(char * msg)
{
    printf("Line %d: %s\n", yylineno, msg);
}



