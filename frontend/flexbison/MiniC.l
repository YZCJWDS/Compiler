%{
/* 这里声明语义动作符程序所需要的函数原型或者变量原型或定义等 */
/* 主要包含头文件，extern的全局变量，定义的全局变量等 */

#include <cstdio>
#include <cstdint>
#include <cstring>

// 此文件定义了文法中终结符的类别
#include "BisonParser.h"

// 对于整数或浮点数，词法识别无符号数，对于负数，识别为求负运算符与无符号数，请注意。
%}

/* 使它不要添加默认的规则,这样输入无法被给定的规则完全匹配时，词法分析器可以报告一个错误 */
%option nodefault

/* 产生yywrap函数 */
%option noyywrap

/* flex 生成的扫描器用全局变量yylineno 维护着输入文件的当前行编号 */
%option yylineno

/* 区分大小写 */
%option case-sensitive

/* yytext的类型为指针类型，即char * */
%option pointer

/* 生成可重用的扫描器API，这些API用于多线程环境 */
/* %option reentrant */

/* 不进行命令行交互，只能分析文件 */
%option never-interactive

/* 辅助定义式或者宏，后面使用时带上大括号 */

/* 正规式定义 */
%%

"("         { return T_L_PAREN; }
")"         { return T_R_PAREN; }
"{"         { return T_L_BRACE; }
"}"         { return T_R_BRACE; }
"["         { return T_L_BRACKET; }
"]"         { return T_R_BRACKET; }

";"         { return T_SEMICOLON; }
","         { return T_COMMA; }

"="         { return T_ASSIGN; }
"+"         { return T_ADD; }
"-"         { return T_SUB; }
"*"         { return T_MUL; }
"/"         { return T_DIV; }
"%"         { return T_MOD; }

"!"       {
                // 逻辑非运算符
                return T_NOT;
            }

">="      {
                // 大于等于运算符
                return T_GE;
            }

">"       {
                // 大于运算符
                return T_GT;
            }

"<="      {
                // 小于等于运算符
                return T_LE;
            }

"<"       {
                // 小于运算符
                return T_LT;
            }

"=="      {
                // 等于运算符
                return T_EQ;
            }

"!="      {
                // 不等于运算符
                return T_NE;
            }

0[xX][0-9a-fA-F]+ {
    yylval.integer_num.val = (uint32_t)strtoul(yytext, NULL, 16);
    yylval.integer_num.lineno = yylineno;
    return T_DIGIT;
}
0[0-7]* {
    yylval.integer_num.val = (uint32_t)strtoul(yytext, NULL, 8);
    yylval.integer_num.lineno = yylineno;
    return T_DIGIT;
}
[1-9][0-9]* {
    yylval.integer_num.val = (uint32_t)strtoul(yytext, NULL, 10);
    yylval.integer_num.lineno = yylineno;
    return T_DIGIT;
}

"int"       {
                // int类型关键字 关键字的识别要在标识符识别的前边，这是因为关键字也是标识符，不过是保留的
                yylval.type.type = BasicType::TYPE_INT;
                yylval.type.lineno = yylineno;
                return T_INT;
            }

"void"      {
                // void类型关键字
                yylval.type.type = BasicType::TYPE_VOID;
                yylval.type.lineno = yylineno;
                return T_VOID;
            }

"return"    {
                // return关键字 关键字的识别要在标识符识别的前边，，这是因为关键字也是标识符，不过是保留的
                return T_RETURN;
            }

"if"      {
                // if关键字
                return T_IF;
            }

"else"    {
                // else关键字
                return T_ELSE;
            }

"while"   {
                // while关键字
                return T_WHILE;
            }

"break"   {
                // break关键字
                return T_BREAK;
            }

"continue"   {
                // continue关键字
                return T_CONTINUE;
            }

"for"     {
                // for关键字
                return T_FOR;
            }

"&&"      {
                // 逻辑与运算符
                return T_AND;
            }

"||"      {
                // 逻辑或运算符
                return T_OR;
            }

[a-zA-Z_]+[0-9a-zA-Z_]* {
                // strdup 分配的空间需要在使用完毕后使用free手动释放，否则会造成内存泄漏
                yylval.var_id.id = strdup(yytext);
                yylval.var_id.lineno = yylineno;
                return T_ID;
            }


[\t\040]+   {
                /* \040代表8进制的32的识别，也就是空格字符 */
                // 空白符号忽略
                ;
            }

[\r\n]+     {
                // 空白行忽略
                ;
            }

"//".*      {
                // 单行注释，直接跳过
                ;
            }

"/*"        {
                // 多行注释开始，进入注释处理状态
                int c;
                while ((c = yyinput()) != 0) {
                    if (c == '*') {
                        if ((c = yyinput()) == '/') {
                            break;  // 找到注释结束标记
                        } else {
                            unput(c);  // 回退一个字符
                        }
                    }
                }
            }

.           {
                printf("Line %d: Invalid char %s\n", yylineno, yytext);
                // 词法识别错误
                return 257;
            }
%%
