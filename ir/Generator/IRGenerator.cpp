/// @file IRGenerator.cpp
/// @brief AST遍历产生线性IR的源文件
/// @author zenglj (zenglj@live.com)
/// @version 1.1
/// @date 2024-11-23
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-09-29 <td>1.0     <td>zenglj  <td>新建
/// <tr><td>2024-11-23 <td>1.1     <td>zenglj  <td>表达式版增强
/// </table>
///
#include <cstdint>
#include <cstdio>
#include <unordered_map>
#include <vector>
#include <iostream>

#include "AST.h"
#include "Common.h"
#include "Function.h"
#include "IRCode.h"
#include "IRGenerator.h"
#include "Module.h"
#include "EntryInstruction.h"
#include "LabelInstruction.h"
#include "ExitInstruction.h"
#include "FuncCallInstruction.h"
#include "BinaryInstruction.h"
#include "MoveInstruction.h"
#include "GotoInstruction.h"
#include "Instruction.h"
#include "ConstInt.h"
#include "PointerType.h"
#include "IntegerType.h"

/// @brief 构造函数
/// @param _root AST的根
/// @param _module 符号表
IRGenerator::IRGenerator(ast_node * _root, Module * _module) : root(_root), module(_module)
{
    /* 叶子节点 */
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_LITERAL_UINT] = &IRGenerator::ir_leaf_node_uint;
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_VAR_ID] = &IRGenerator::ir_leaf_node_var_id;
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_TYPE] = &IRGenerator::ir_leaf_node_type;

    /* 表达式运算， 加减 */
    ast2ir_handlers[ast_operator_type::AST_OP_SUB] = &IRGenerator::ir_sub;
    ast2ir_handlers[ast_operator_type::AST_OP_ADD] = &IRGenerator::ir_add;

    ast2ir_handlers[ast_operator_type::AST_OP_MUL] = &IRGenerator::ir_mul;
    ast2ir_handlers[ast_operator_type::AST_OP_DIV] = &IRGenerator::ir_div;
    ast2ir_handlers[ast_operator_type::AST_OP_MOD] = &IRGenerator::ir_mod;
    ast2ir_handlers[ast_operator_type::AST_OP_NEG] = &IRGenerator::ir_neg;
    ast2ir_handlers[ast_operator_type::AST_OP_NOT] = &IRGenerator::ir_not;

    /* 语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_ASSIGN] = &IRGenerator::ir_assign;
    ast2ir_handlers[ast_operator_type::AST_OP_RETURN] = &IRGenerator::ir_return;

    /* 函数调用 */
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_CALL] = &IRGenerator::ir_function_call;

    /* 函数定义 */
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_DEF] = &IRGenerator::ir_function_define;
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS] = &IRGenerator::ir_function_formal_params;

    /* 变量定义语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_DECL_STMT] = &IRGenerator::ir_declare_statment;
    ast2ir_handlers[ast_operator_type::AST_OP_VAR_DECL] = &IRGenerator::ir_variable_declare;

    /* 语句块 */
    ast2ir_handlers[ast_operator_type::AST_OP_BLOCK] = &IRGenerator::ir_block;

    /* 编译单元 */
    ast2ir_handlers[ast_operator_type::AST_OP_COMPILE_UNIT] = &IRGenerator::ir_compile_unit;
    // 在IRGenerator构造函数中添加
    // 使用更简单的比较操作符格式
    ast2ir_handlers[ast_operator_type::AST_OP_GT] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_LT] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_GE] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_LE] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_EQ] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_NE] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_LOGICAL_OR] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_LOGICAL_AND] = &IRGenerator::ir_compare_op;
    ast2ir_handlers[ast_operator_type::AST_OP_IF] = &IRGenerator::ir_if;
    ast2ir_handlers[ast_operator_type::AST_OP_CONDITIONAL] = &IRGenerator::ir_if;
    ast2ir_handlers[ast_operator_type::AST_OP_LOOP] = &IRGenerator::ir_loop;
    ast2ir_handlers[ast_operator_type::AST_OP_BREAK] = &IRGenerator::ir_break;
    ast2ir_handlers[ast_operator_type::AST_OP_CONTINUE] = &IRGenerator::ir_continue;

    /* 数组操作 */
    ast2ir_handlers[ast_operator_type::AST_OP_ARRAY_ACCESS] = &IRGenerator::ir_array_access;
}

/// @brief 遍历抽象语法树产生线性IR，保存到IRCode中
/// @param root 抽象语法树
/// @param IRCode 线性IR
/// @return true: 成功 false: 失败
bool IRGenerator::run()
{
    ast_node * node;

    // 从根节点进行遍历
    node = ir_visit_ast_node(root);

    return node != nullptr;
}

/// @brief 根据AST的节点运算符查找对应的翻译函数并执行翻译动作
/// @param node AST节点
/// @return 成功返回node节点，否则返回nullptr
ast_node * IRGenerator::ir_visit_ast_node(ast_node * node)
{
    // 空节点
    if (nullptr == node) {
        return nullptr;
    }

    bool result;

    std::unordered_map<ast_operator_type, ast2ir_handler_t>::const_iterator pIter;
    pIter = ast2ir_handlers.find(node->node_type);
    if (pIter == ast2ir_handlers.end()) {
        // 没有找到，则说明当前不支持
        result = (this->ir_default)(node);
    } else {
        result = (this->*(pIter->second))(node);
    }

    if (!result) {
        // 语义解析错误，则出错返回
        node = nullptr;
    }

    return node;
}

/// @brief 未知节点类型的节点处理
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_default(ast_node * node)
{
    // 未知的节点
    printf("Unkown node(%d)\n", (int) node->node_type);
    return true;
}

/// @brief 编译单元AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_compile_unit(ast_node * node)
{
    module->setCurrentFunction(nullptr);

    for (auto son: node->sons) {

        // 遍历编译单元，要么是函数定义，要么是语句
        ast_node * son_node = ir_visit_ast_node(son);
        if (!son_node) {
            // TODO 自行追加语义错误处理
            return false;
        }
    }

    return true;
}

/// @brief 函数定义AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_define(ast_node * node)
{
    bool result;

    // 创建一个函数，用于当前函数处理
    if (module->getCurrentFunction()) {
        // 函数中嵌套定义函数，这是不允许的，错误退出
        // TODO 自行追加语义错误处理
        return false;
    }

    // 函数定义的AST包含四个孩子
    // 第一个孩子：函数返回类型
    // 第二个孩子：函数名字
    // 第三个孩子：形参列表
    // 第四个孩子：函数体即block
    ast_node * type_node = node->sons[0];
    ast_node * name_node = node->sons[1];
    ast_node * param_node = node->sons[2];
    ast_node * block_node = node->sons[3];

    // 获取函数名
    std::string funcName = name_node->name;

    // 处理形参列表，构建形参类型列表
    std::vector<FormalParam *> formalParams;
    if (param_node && param_node->node_type == ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS) {
        for (auto param: param_node->sons) {
            if (param->node_type == ast_operator_type::AST_OP_FUNC_FORMAL_PARAM && param->sons.size() >= 2) {
                Type * paramType = param->sons[0]->type;
                std::string paramName = param->sons[1]->name;

                // 检查是否是数组参数（有第三个子节点作为数组标记）
                bool isArrayParam = (param->sons.size() >= 3);

                // 对于数组参数，保持原始类型（不转换为指针）
                Type * actualParamType = paramType;

                FormalParam * formalParam = new FormalParam(actualParamType, paramName);

                // 如果是数组参数，设置数组维度信息
                if (isArrayParam) {
                    ast_node * dimensions_node = param->sons[2]; // 第三个子节点是维度信息
                    std::vector<uint32_t> dimensions;

                    // 第一维是0（未知大小）
                    dimensions.push_back(0);

                    if (dimensions_node && dimensions_node->node_type == ast_operator_type::AST_OP_ARRAY_DIMENSIONS) {
                        // 遍历所有维度子节点，跳过第一个（已经添加0）
                        for (size_t i = 1; i < dimensions_node->sons.size(); i++) {
                            ast_node * dim_node = dimensions_node->sons[i];
                            if (dim_node->node_type == ast_operator_type::AST_OP_LEAF_LITERAL_UINT) {
                                dimensions.push_back(dim_node->integer_val);
                            } else {
                                minic_log(LOG_ERROR, "数组参数维度必须是整数常量");
                                return false;
                            }
                        }
                    }

                    formalParam->setArrayDimensions(dimensions);
                }

                formalParams.push_back(formalParam);
            }
        }
    }

    // 创建一个新的函数定义，包含参数列表
    Function * newFunc = module->newFunction(funcName, type_node->type, formalParams);
    if (!newFunc) {
        // 新定义的函数已经存在，则失败返回。
        // TODO 自行追加语义错误处理
        return false;
    }

    // 当前函数设置有效，变更为当前的函数
    module->setCurrentFunction(newFunc);

    // 进入函数的作用域
    module->enterScope();
    module->enterScope();

    // 获取函数的IR代码列表，用于后面追加指令用，注意这里用的是引用传值
    InterCode & irCode = newFunc->getInterCode();

    // 创建函数入口标签
    LabelInstruction * entryLabelInst = new LabelInstruction(newFunc, ".L1");
    irCode.addInst(entryLabelInst);

    // 创建并加入Entry入口指令
    irCode.addInst(new EntryInstruction(newFunc));

    // 创建出口指令并不加入出口指令，等函数内的指令处理完毕后加入出口指令
    LabelInstruction * exitLabelInst = new LabelInstruction(newFunc, ".L2");

    // 函数出口指令保存到函数信息中，因为在语义分析函数体时return语句需要跳转到函数尾部，需要这个label指令
    newFunc->setExitLabel(exitLabelInst);

    // 遍历形参，创建局部变量
    if (param_node) {
        result = ir_function_formal_params(param_node);
        if (!result) {
            // 形参解析失败
            return false;
        }
        irCode.addInst(param_node->blockInsts);
    }

    // 新建一个Value，用于保存函数的返回值，如果没有返回值可不用申请
    LocalVariable * retValue = nullptr;
    if (!type_node->type->isVoidType()) {
        // 保存函数返回值变量到函数信息中，在return语句翻译时需要设置值到这个变量中
        retValue = static_cast<LocalVariable *>(module->newVarValue(type_node->type));

        // 对于main函数，需要初始化返回值为0
        if (funcName == "main") {
            // 创建一个常量0
            Value * zeroConst = module->newConstInt(0);

            // 创建一个赋值指令，将0赋值给返回值变量
            MoveInstruction * moveInst = new MoveInstruction(newFunc, retValue, zeroConst);

            // 将指令添加到当前函数的IR代码中
            irCode.addInst(moveInst);
        }
    }
    newFunc->setReturnValue(retValue);

    // 函数内已经进入作用域，内部不再需要做变量的作用域管理
    block_node->needScope = false;

    // 遍历block
    result = ir_block(block_node);
    if (!result) {
        // block解析失败
        // TODO 自行追加语义错误处理
        return false;
    }

    // 将block节点的指令直接添加到irCode中，而不是先添加到node->blockInsts
    irCode.addInst(block_node->blockInsts);

    // 添加函数出口Label指令，主要用于return语句跳转到这里进行函数的退出
    irCode.addInst(exitLabelInst);

    // 函数出口指令
    irCode.addInst(new ExitInstruction(newFunc, retValue));

    // 恢复成外部函数
    module->setCurrentFunction(nullptr);

    // 退出函数的作用域
    module->leaveScope();

    return true;
}

/// @brief 形式参数AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_formal_params(ast_node * node)
{
    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "形参处理必须在函数内部");
        return false;
    }

    // 如果节点为空，则无需处理
    if (!node || node->sons.empty()) {
        return true;
    }

    // 遍历所有形参节点
    for (auto param_node: node->sons) {
        if (param_node->node_type != ast_operator_type::AST_OP_FUNC_FORMAL_PARAM) {
            minic_log(LOG_ERROR, "形参节点类型错误");
            return false;
        }

        // 形参节点包含两个或三个子节点：
        // 2个子节点：类型节点和变量名节点（普通参数）
        // 3个子节点：类型节点、变量名节点和数组维度节点（数组参数）
        if (param_node->sons.size() != 2 && param_node->sons.size() != 3) {
            minic_log(LOG_ERROR, "形参节点子节点数量错误");
            return false;
        }

        ast_node * type_node = param_node->sons[0];
        ast_node * name_node = param_node->sons[1];
        bool isArrayParam = (param_node->sons.size() == 3);

        // 获取形参类型和名称
        Type * paramType = type_node->type;
        std::string paramName = name_node->name;

        // 获取对应的形参实例 (之前在 ir_function_define 中已经创建并添加到函数中)
        FormalParam * formalParam = nullptr;
        for (auto & param: currentFunc->getParams()) {
            if (param->getName() == paramName) {
                formalParam = param;
                break;
            }
        }

        if (!formalParam) {
            minic_log(LOG_ERROR, "形参 %s 未在函数中定义", paramName.c_str());
            return false;
        }

        // 创建形参对应的局部变量 - 这是函数体内部使用的变量
        LocalVariable * localVar = static_cast<LocalVariable *>(module->newVarValue(paramType, paramName));
        if (!localVar) {
            minic_log(LOG_ERROR, "无法创建形参对应的局部变量 %s", paramName.c_str());
            return false;
        }

        // 如果是数组参数，设置数组维度信息
        if (isArrayParam) {
            ast_node * dimensions_node = param_node->sons[2]; // 第三个子节点是维度信息
            std::vector<uint32_t> dimensions;

            // 第一维是0（未知大小），从维度节点获取后续维度
            dimensions.push_back(0);

            if (dimensions_node && dimensions_node->node_type == ast_operator_type::AST_OP_ARRAY_DIMENSIONS) {
                // 遍历所有维度子节点，跳过第一个（已经添加0）
                for (size_t i = 1; i < dimensions_node->sons.size(); i++) {
                    ast_node * dim_node = dimensions_node->sons[i];
                    if (dim_node->node_type == ast_operator_type::AST_OP_LEAF_LITERAL_UINT) {
                        dimensions.push_back(dim_node->integer_val);
                    } else {
                        minic_log(LOG_ERROR, "数组参数维度必须是整数常量");
                        return false;
                    }
                }
            }

            localVar->setArrayDimensions(dimensions);
        }

        // 创建赋值指令，将形参值拷贝到局部变量
        MoveInstruction * moveInst = new MoveInstruction(currentFunc, localVar, formalParam);

        // 将指令添加到形参节点的指令序列中
        node->blockInsts.addInst(moveInst);
    }

    return true;
}

/// @brief 函数调用AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_call(ast_node * node)
{
    std::vector<Value *> realParams;

    // 获取当前正在处理的函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "函数调用必须在函数内部");
        return false;
    }

    // 函数调用的节点包含两个节点：
    // 第一个节点：函数名节点
    // 第二个节点：实参列表节点
    if (node->sons.size() < 2) {
        minic_log(LOG_ERROR, "函数调用节点格式错误");
        return false;
    }

    std::string funcName = node->sons[0]->name;
    int64_t lineno = node->sons[0]->line_no;

    ast_node * paramsNode = node->sons[1];

    // 根据函数名查找函数，看是否存在。若不存在则出错
    auto calledFunction = module->findFunction(funcName);
    if (nullptr == calledFunction) {
        minic_log(LOG_ERROR, "函数(%s)未定义或声明", funcName.c_str());
        return false;
    }

    // 当前函数存在函数调用
    currentFunc->setExistFuncCall(true);

    // 处理实参
    if (paramsNode && paramsNode->node_type == ast_operator_type::AST_OP_FUNC_REAL_PARAMS) {
        int32_t argsCount = (int32_t) paramsNode->sons.size();

        // 当前函数中调用函数实参个数最大值统计，实际上是统计实参传参需在栈中分配的大小
        if (argsCount > currentFunc->getMaxFuncCallArgCnt()) {
            currentFunc->setMaxFuncCallArgCnt(argsCount);
        }

        // 遍历参数列表，孩子是表达式
        for (auto son: paramsNode->sons) {
            // 处理每个实参表达式
            ast_node * temp = ir_visit_ast_node(son);
            if (!temp || !temp->val) {
                minic_log(LOG_ERROR, "实参表达式错误");
                return false;
            }

            realParams.push_back(temp->val);
            node->blockInsts.addInst(temp->blockInsts);
        }
    }

    // 检查函数参数的个数是否匹配
    const auto & formalParams = calledFunction->getParams();
    if (realParams.size() != formalParams.size()) {
        minic_log(LOG_ERROR,
                  "第%lld行的函数调用(%s)参数个数不匹配：预期%zu个，实际%zu个",
                  (long long) lineno,
                  funcName.c_str(),
                  formalParams.size(),
                  realParams.size());
        return false;
    }

    // 检查参数类型是否匹配
    for (size_t i = 0; i < realParams.size(); i++) {
        if (!realParams[i] || !formalParams[i]) {
            minic_log(LOG_ERROR, "第%lld行的函数调用(%s)第%zu个参数为空", (long long) lineno, funcName.c_str(), i + 1);
            return false;
        }

        Type * realType = realParams[i]->getType();
        Type * formalType = formalParams[i]->getType();

        minic_log(LOG_DEBUG,
                  "参数%zu比较: 实参类型=%s(指针=%p), 形参类型=%s(指针=%p), 相等=%s",
                  i + 1,
                  realType->toString().c_str(),
                  realType,
                  formalType->toString().c_str(),
                  formalType,
                  (realType == formalType) ? "true" : "false");

        // 检查类型是否匹配，需要考虑数组参数的特殊情况
        bool typesMatch = (realType == formalType);

        // 如果直接比较不匹配，检查是否是数组参数的情况
        if (!typesMatch) {
            // 检查是否是数组传递给数组参数的情况
            // 实参是指针类型，形参是基本类型（数组参数退化前的类型）
            if (realType->isPointerType() && !formalType->isPointerType()) {
                const PointerType * realPtrType = dynamic_cast<const PointerType *>(realType);
                if (realPtrType && realPtrType->getPointeeType() == formalType) {
                    typesMatch = true;
                    minic_log(LOG_DEBUG,
                              "参数%zu: 数组实参(%s)匹配数组形参(%s)",
                              i + 1,
                              realType->toString().c_str(),
                              formalType->toString().c_str());
                }
            }
        }

        if (!typesMatch) {
            minic_log(LOG_ERROR,
                      "第%lld行的函数调用(%s)第%zu个参数类型不匹配: 实参类型=%s, 形参类型=%s",
                      (long long) lineno,
                      funcName.c_str(),
                      i + 1,
                      realType->toString().c_str(),
                      formalType->toString().c_str());
            return false;
        }
    }

    // 获取返回类型
    Type * type = calledFunction->getReturnType();

    // 创建函数调用指令
    FuncCallInstruction * funcCallInst = new FuncCallInstruction(currentFunc, calledFunction, realParams, type);
    node->blockInsts.addInst(funcCallInst);

    // 函数调用结果Value保存到node中
    node->val = funcCallInst;

    return true;
}

/// @brief 语句块（含函数体）AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_block(ast_node * node)
{
    // 进入作用域
    if (node->needScope) {
        module->enterScope();
    }

    std::vector<ast_node *>::iterator pIter;
    for (pIter = node->sons.begin(); pIter != node->sons.end(); ++pIter) {

        // 遍历Block的每个语句，进行显示或者运算
        ast_node * temp = ir_visit_ast_node(*pIter);
        if (!temp) {
            return false;
        }

        node->blockInsts.addInst(temp->blockInsts);
    }

    // 离开作用域
    if (node->needScope) {
        module->leaveScope();
    }

    return true;
}

/// @brief 整数加法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_add(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 加法节点，左结合，先计算左节点，后计算右节点

    // 加法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 加法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "加法操作必须在函数内部");
        return false;
    }

    // 添加操作数的指令序列
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);

    Value * leftVal = left->val;
    Value * rightVal = right->val;

    // 检查操作数类型，如果有布尔类型需要转换为整数
    if (leftVal->getType()->isInt1Byte()) {
        // 左操作数是布尔类型，需要转换为整数
        LabelInstruction * trueLabel = new LabelInstruction(currentFunc);
        LabelInstruction * falseLabel = new LabelInstruction(currentFunc);
        LabelInstruction * endLabel = new LabelInstruction(currentFunc);

        // 创建整数变量存储转换结果
        LocalVariable * intVar = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeInt()));

        // 条件分支：检查布尔值
        BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                               IRInstOperator::IRINST_OP_BC,
                                                               leftVal,
                                                               trueLabel,
                                                               IntegerType::getTypeBool());
        branchInst->addOperand(falseLabel);
        node->blockInsts.addInst(branchInst);

        // 当布尔值为true时，整数值为1
        node->blockInsts.addInst(trueLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, intVar, module->newConstInt(1)));
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));

        // 当布尔值为false时，整数值为0
        node->blockInsts.addInst(falseLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, intVar, module->newConstInt(0)));

        // 结束标签
        node->blockInsts.addInst(endLabel);

        // 更新左操作数为转换后的整数值
        leftVal = intVar;
    }

    if (rightVal->getType()->isInt1Byte()) {
        // 右操作数是布尔类型，需要转换为整数
        LabelInstruction * trueLabel = new LabelInstruction(currentFunc);
        LabelInstruction * falseLabel = new LabelInstruction(currentFunc);
        LabelInstruction * endLabel = new LabelInstruction(currentFunc);

        // 创建整数变量存储转换结果
        LocalVariable * intVar = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeInt()));

        // 条件分支：检查布尔值
        BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                               IRInstOperator::IRINST_OP_BC,
                                                               rightVal,
                                                               trueLabel,
                                                               IntegerType::getTypeBool());
        branchInst->addOperand(falseLabel);
        node->blockInsts.addInst(branchInst);

        // 当布尔值为true时，整数值为1
        node->blockInsts.addInst(trueLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, intVar, module->newConstInt(1)));
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));

        // 当布尔值为false时，整数值为0
        node->blockInsts.addInst(falseLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, intVar, module->newConstInt(0)));

        // 结束标签
        node->blockInsts.addInst(endLabel);

        // 更新右操作数为转换后的整数值
        rightVal = intVar;
    }

    // 现在执行整数加法
    BinaryInstruction * addInst = new BinaryInstruction(currentFunc,
                                                        IRInstOperator::IRINST_OP_ADD_I,
                                                        leftVal,
                                                        rightVal,
                                                        IntegerType::getTypeInt());

    node->blockInsts.addInst(addInst);
    node->val = addInst;

    return true;
}

/// @brief 整数减法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_sub(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 加法节点，左结合，先计算左节点，后计算右节点

    // 加法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 加法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * subInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_SUB_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(subInst);

    node->val = subInst;

    return true;
}

/// @brief 整数乘法AST节点翻译成线性中间IR
bool IRGenerator::ir_mul(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false;

    BinaryInstruction * mulInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_MUL_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(mulInst);
    node->val = mulInst;

    return true;
}

/// @brief 整数取模AST节点翻译成线性中间IR
bool IRGenerator::ir_mod(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false;

    BinaryInstruction * modInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_MOD_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(modInst);
    node->val = modInst;

    return true;
}

/// @brief 整数除法AST节点翻译成线性中间IR
bool IRGenerator::ir_div(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right)
        return false;

    BinaryInstruction * divInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_DIV_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(divInst);
    node->val = divInst;

    return true;
}

/// @brief 整数取反AST节点翻译成线性中间IR
bool IRGenerator::ir_neg(ast_node * node)
{
    // 参数检查
    if (!node || node->sons.empty()) {
        return false;
    }

    // 处理操作数AST节点
    ast_node * operand_node = node->sons[0];
    ast_node * operand = ir_visit_ast_node(operand_node);
    if (!operand) {
        return false;
    }

    // 获取操作数的值，确保不为空
    Value * operandValue = operand->val;
    if (!operandValue) {
        minic_log(LOG_ERROR, "操作数值为空");
        return false;
    }

    // 检查是否是常量表达式（用于全局变量初始化）
    ConstInt * constOperand = dynamic_cast<ConstInt *>(operandValue);
    if (constOperand) {
        // 常量取反，直接计算结果
        int32_t result = -constOperand->getVal();
        ConstInt * constResult = new ConstInt(result);
        node->val = constResult;
        // 常量表达式不需要生成指令
        return true;
    }

    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "取反操作必须在函数内部");
        return false;
    }

    // 首先添加操作数的指令序列
    node->blockInsts.addInst(operand->blockInsts);

    // 获取操作数类型
    Type * operandType = operandValue->getType();
    if (!operandType) {
        minic_log(LOG_ERROR, "操作数类型为空");
        return false;
    }

    // 如果是布尔类型(i1)，需要特殊处理
    if (operandType->isInt1Byte()) {
        // 在C语言中：
        // - 如果布尔值为true(1)，取负后是-1
        // - 如果布尔值为false(0)，取负后是0

        // 创建选择指令：根据操作数的布尔值选择不同的整数值
        LabelInstruction * trueLabel = new LabelInstruction(currentFunc);
        LabelInstruction * falseLabel = new LabelInstruction(currentFunc);
        LabelInstruction * endLabel = new LabelInstruction(currentFunc);

        // 创建存储结果的整数变量
        LocalVariable * result = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeInt()));
        if (!result) {
            minic_log(LOG_ERROR, "无法创建临时变量");
            return false;
        }

        // 构建条件分支：检查布尔值
        BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                               IRInstOperator::IRINST_OP_BC,
                                                               operandValue,
                                                               trueLabel,
                                                               IntegerType::getTypeBool());
        branchInst->addOperand(falseLabel);
        node->blockInsts.addInst(branchInst);

        // 当布尔值为true时，结果为-1
        node->blockInsts.addInst(trueLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, result, module->newConstInt(-1)));
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));

        // 当布尔值为false时，结果为0
        node->blockInsts.addInst(falseLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, result, module->newConstInt(0)));

        // 结束标签
        node->blockInsts.addInst(endLabel);

        // 设置结果
        node->val = result;
        return true;
    } else if (!operandType->isIntegerType()) {
        // 确保操作数是整数类型
        minic_log(LOG_ERROR, "取反操作要求整数操作数");
        return false;
    }

    // 对整数类型取负
    BinaryInstruction * negInst =
        new BinaryInstruction(currentFunc, IRInstOperator::IRINST_OP_NEG_I, operandValue, operandValue, operandType);

    // 添加取反指令
    node->blockInsts.addInst(negInst);
    node->val = negInst;

    return true;
}

/// @brief 逻辑非AST节点翻译成线性中间IR
bool IRGenerator::ir_not(ast_node * node)
{
    // 参数检查
    if (!node || node->sons.empty()) {
        return false;
    }

    // 处理操作数AST节点
    ast_node * operand_node = node->sons[0];
    ast_node * operand = ir_visit_ast_node(operand_node);
    if (!operand) {
        return false;
    }

    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "逻辑非操作必须在函数内部");
        return false;
    }

    // 将操作数转换为布尔值，逻辑非操作应该是：
    // 1. 如果操作数为0，结果为1
    // 2. 如果操作数不为0，结果为0

    Value * operandValue = operand->val;
    BinaryInstruction * cmpInst = nullptr;

    // 创建相等比较指令：operandValue == 0
    cmpInst = new BinaryInstruction(currentFunc,
                                    IRInstOperator::IRINST_OP_EQ_I,
                                    operandValue,
                                    module->newConstInt(0),
                                    IntegerType::getTypeBool());

    // 将指令添加到节点的指令序列中
    node->blockInsts.addInst(operand->blockInsts);
    node->blockInsts.addInst(cmpInst);
    node->val = cmpInst;

    return true;
}

/// @brief 赋值AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_assign(ast_node * node)
{
    ast_node * son1_node = node->sons[0];
    ast_node * son2_node = node->sons[1];

    // 赋值节点，自右往左运算

    // 赋值运算符的左侧操作数
    ast_node * left = ir_visit_ast_node(son1_node);
    if (!left) {
        // 某个变量没有定值
        // 这里缺省设置变量不存在则创建，因此这里不会错误
        return false;
    }

    // 赋值运算符的右侧操作数
    ast_node * right = ir_visit_ast_node(son2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    MoveInstruction * movInst = new MoveInstruction(module->getCurrentFunction(), left->val, right->val);

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(movInst);

    // 这里假定赋值的类型是一致的
    node->val = movInst;

    return true;
}

/// @brief return节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_return(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    if (!currentFunc) {
        minic_log(LOG_ERROR, "return语句必须在函数内部");
        return false;
    }

    // 检查函数的返回类型
    Type * returnType = currentFunc->getReturnType();
    bool isVoidReturn = returnType->isVoidType();

    // 获取返回值变量
    LocalVariable * returnValue = currentFunc->getReturnValue();

    // return语句可能没有没有表达式，也可能有，因此这里必须进行区分判断
    if (!node->sons.empty()) {
        // 有返回表达式的情况
        ast_node * expr_node = node->sons[0];

        // 如果函数返回类型是void，但有返回表达式，则报错
        if (isVoidReturn) {
            minic_log(LOG_ERROR, "void类型函数不能有返回值");
            return false;
        }

        // 如果返回值变量未设置，则报错
        if (!returnValue) {
            minic_log(LOG_ERROR, "函数返回值变量未设置");
            return false;
        }

        // 处理返回表达式
        ast_node * expr_result = ir_visit_ast_node(expr_node);
        if (!expr_result || !expr_result->val) {
            minic_log(LOG_ERROR, "返回表达式错误");
            return false;
        }

        // 返回值存在时则移动指令到node中
        node->blockInsts.addInst(expr_result->blockInsts);

        // 返回值赋值到函数返回值变量上
        // 对于main函数的return 0，检查是否已经有相同的初始化赋值，避免重复
        bool needAssignment = true;
        if (currentFunc->getName() == "main") {
            ConstInt * constVal = dynamic_cast<ConstInt *>(expr_result->val);
            if (constVal && constVal->getVal() == 0) {
                // 这是main函数的return 0，检查是否已经有相同的赋值
                // 遍历当前函数的指令列表，查找是否已存在相同的赋值
                auto & insts = currentFunc->getInterCode().getInsts();
                for (auto inst: insts) {
                    if (auto moveInst = dynamic_cast<MoveInstruction *>(inst)) {
                        if (moveInst->getOperand(0) == returnValue) {
                            ConstInt * srcConst = dynamic_cast<ConstInt *>(moveInst->getOperand(1));
                            if (srcConst && srcConst->getVal() == 0) {
                                // 已经存在 %l0 = 0 的赋值，跳过此次赋值
                                needAssignment = false;
                                break;
                            }
                        }
                    }
                }
            }
        }

        if (needAssignment) {
            node->blockInsts.addInst(new MoveInstruction(currentFunc, returnValue, expr_result->val));
        }

        // 保存返回值
        node->val = expr_result->val;
    } else {
        // 没有返回表达式的情况

        // 如果函数返回类型不是void，但没有返回表达式，则报错
        if (!isVoidReturn) {
            minic_log(LOG_ERROR, "非void类型函数必须有返回值");
            return false;
        }

        // void函数的返回值为null
        node->val = nullptr;
    }

    // 跳转到函数的尾部出口指令上
    node->blockInsts.addInst(new GotoInstruction(currentFunc, currentFunc->getExitLabel()));

    return true;
}

/// @brief 类型叶子节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_type(ast_node * node)
{
    // 不需要做什么，直接从节点中获取即可。

    return true;
}

/// @brief 标识符叶子节点翻译成线性中间IR，变量声明的不走这个语句
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_var_id(ast_node * node)
{
    Value * val;

    // 查找ID型Value
    // 变量，则需要在符号表中查找对应的值
    val = module->findVarValue(node->name);

    if (!val) {
        // 如果在变量表中找不到，检查是否是函数名
        Function * func = module->findFunction(node->name);
        if (func) {
            minic_log(LOG_ERROR, "第%lld行：试图将函数名 '%s' 用作变量", (long long) node->line_no, node->name.c_str());
            return false;
        }

        minic_log(LOG_ERROR, "第%lld行的变量(%s)未定义或声明", (long long) node->line_no, node->name.c_str());
        return false;
    }

    // 检查是否需要进行数组到指针的转换
    // 需要进行转换的情况：
    // 1. 变量是数组类型
    // 2. 在函数调用的实参上下文中
    bool needArrayToPointerConversion = false;
    
    // 检查变量是否为数组
    bool isArrayVar = false;
    LocalVariable * localVar = dynamic_cast<LocalVariable *>(val);
    GlobalVariable * globalVar = dynamic_cast<GlobalVariable *>(val);
    FormalParam * formalParam = dynamic_cast<FormalParam *>(val);
    
    if (localVar && localVar->isArray()) {
        isArrayVar = true;
    } else if (globalVar && globalVar->isArray()) {
        isArrayVar = true;
    } else if (formalParam && formalParam->isArray()) {
        isArrayVar = true;
    }
    
    // 如果是数组变量，检查是否在函数调用的上下文中
    if (isArrayVar) {
        // 向上追溯父节点，检查是否在函数调用的实参列表中
        ast_node * current = node->parent;
        while (current) {
            if (current->node_type == ast_operator_type::AST_OP_FUNC_REAL_PARAMS) {
                // 在函数调用的实参列表中，需要进行数组到指针转换
                needArrayToPointerConversion = true;
                break;
            }
            // 继续向上查找
            current = current->parent;
        }
    }
    
    if (needArrayToPointerConversion) {
        // 进行数组到指针转换
        // 数组类型转换为指向数组元素类型的指针
        const Type * elementType = IntegerType::getTypeInt(); // 假设数组元素为int类型
        const PointerType * ptrType = PointerType::get(const_cast<Type *>(elementType));
        
        // 获取当前函数
        Function * currentFunc = module->getCurrentFunction();
        if (!currentFunc) {
            minic_log(LOG_ERROR, "数组到指针转换必须在函数内部");
            return false;
        }
        
        // 创建一个临时指令来表示数组名的地址（数组到指针的转换）
        // 使用数组名 + 0 来获得数组的基地址，这实现了数组到指针的转换
        Instruction * arrayPtrInst = new BinaryInstruction(currentFunc,
                                                          IRInstOperator::IRINST_OP_ADD_I,
                                                          val,
                                                          module->newConstInt(0),
                                                          const_cast<Type *>(static_cast<const Type *>(ptrType)));
        
        node->blockInsts.addInst(arrayPtrInst);
        node->val = arrayPtrInst;
        
        minic_log(LOG_DEBUG, "数组变量 %s 在函数调用中转换为指针类型", node->name.c_str());
    } else {
        // 直接使用变量，不进行数组到指针的转换
        node->val = val;
    }

    return true;
}

/// @brief 无符号整数字面量叶子节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_uint(ast_node * node)
{
    ConstInt * val;

    // 新建一个整数常量Value
    val = module->newConstInt((int32_t) node->integer_val);

    node->val = val;

    return true;
}

/// @brief 变量声明语句节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_declare_statment(ast_node * node)
{
    for (auto & child: node->sons) {

        // 遍历每个变量声明
        bool result = ir_variable_declare(child);
        if (!result) {
            return false; // 直接返回错误
        }

        // 将子节点生成的指令添加到当前节点的指令块中
        node->blockInsts.addInst(child->blockInsts);
    }

    return true; // 所有变量声明都成功
}

/// @brief 变量定声明节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_variable_declare(ast_node * node)
{
    if (node->sons.size() < 2) {
        minic_log(LOG_ERROR, "变量声明节点子节点不足");
        return false;
    }

    // 获取变量的类型和ID节点
    Type * varType = node->sons[0]->type;
    ast_node * idNode = node->sons[1];
    std::string varName;
    ast_node * initExprNode = nullptr;
    std::vector<uint32_t> dimensions; // 数组维度信息，移到更大作用域

    // 提取变量名和初始化表达式
    if (idNode->node_type == ast_operator_type::AST_OP_LEAF_VAR_ID) {
        // 普通变量声明: int a;
        varName = idNode->name;
        // minic_log(LOG_INFO, "处理变量声明：%s（无初始化）", varName.c_str());
    } else if (idNode->node_type == ast_operator_type::AST_OP_ASSIGN) {
        // 带初始化的变量声明: int a = 0;
        if (idNode->sons.size() < 2) {
            minic_log(LOG_ERROR, "赋值节点子节点不足");
            return false;
        }

        // 第一个子节点是变量ID
        if (idNode->sons[0]->node_type != ast_operator_type::AST_OP_LEAF_VAR_ID) {
            minic_log(LOG_ERROR, "非法的变量赋值节点");
            return false;
        }

        varName = idNode->sons[0]->name;
        // 第二个子节点是初始化表达式
        initExprNode = idNode->sons[1];
        // minic_log(LOG_INFO, "处理变量声明：%s（带初始化）", varName.c_str());
    } else if (idNode->node_type == ast_operator_type::AST_OP_ARRAY_DEF) {
        // 数组声明: int a[10]; 或 int a[10][20];
        if (idNode->sons.size() != 2) {
            minic_log(LOG_ERROR, "数组声明节点应该有2个子节点：变量ID和维度信息");
            return false;
        }

        // 第一个子节点是变量ID
        if (idNode->sons[0]->node_type != ast_operator_type::AST_OP_LEAF_VAR_ID) {
            minic_log(LOG_ERROR, "非法的数组变量ID节点");
            return false;
        }

        varName = idNode->sons[0]->name;

        // 第二个子节点是维度信息节点
        ast_node * dimensions_node = idNode->sons[1];
        if (dimensions_node->node_type != ast_operator_type::AST_OP_ARRAY_DIMENSIONS) {
            minic_log(LOG_ERROR, "数组维度信息节点类型错误");
            return false;
        }

        // 处理多维数组的大小
        for (auto dim_node: dimensions_node->sons) {
            if (dim_node->node_type != ast_operator_type::AST_OP_LEAF_LITERAL_UINT) {
                minic_log(LOG_ERROR, "数组大小必须是整数字面量");
                return false;
            }
            dimensions.push_back(dim_node->integer_val);
        }

        // 创建数组类型处理
    } else {
        minic_log(LOG_ERROR, "非法的变量声明格式，节点类型: %d", (int) idNode->node_type);
        return false;
    }

    if (varName.empty()) {
        minic_log(LOG_ERROR, "变量名为空");
        return false;
    }

    // 创建变量
    Value * varValue = module->newVarValue(varType, varName);
    if (!varValue) {
        minic_log(LOG_ERROR, "创建变量失败");
        return false;
    }

    // 如果是数组变量，设置维度信息
    if (idNode->node_type == ast_operator_type::AST_OP_ARRAY_DEF) {
        LocalVariable * localVar = dynamic_cast<LocalVariable *>(varValue);
        if (localVar) {
            localVar->setArrayDimensions(dimensions);
        } else {
            GlobalVariable * globalVar = dynamic_cast<GlobalVariable *>(varValue);
            if (globalVar) {
                globalVar->setArrayDimensions(dimensions);
            }
        }
    }

    // 保存变量到节点
    node->val = varValue;

    // 如果有初始化表达式，处理初始化
    if (initExprNode) {
        // minic_log(LOG_INFO, "处理变量 %s 的初始化表达式", varName.c_str());

        // 处理初始化表达式
        ast_node * initValueNode = ir_visit_ast_node(initExprNode);
        if (!initValueNode || !initValueNode->val) {
            minic_log(LOG_ERROR, "变量初始化表达式错误");
            return false;
        }

        // 添加初始化指令
        node->blockInsts.addInst(initValueNode->blockInsts);

        Function * currentFunc = module->getCurrentFunction();

        if (currentFunc) {
            // 局部变量初始化：在函数内部添加赋值指令
            // 创建赋值指令，将初始值赋给变量
            // minic_log(LOG_INFO,
            //           "创建变量 %s 的赋值指令，初始值类型: %s",
            //           varName.c_str(),
            //           initValueNode->val ? initValueNode->val->getType()->toString().c_str() : "未知");

            MoveInstruction * moveInst = new MoveInstruction(currentFunc, varValue, initValueNode->val);
            node->blockInsts.addInst(moveInst);

            // minic_log(LOG_INFO, "变量 %s 初始化完成", varName.c_str());
        } else {
            // 全局变量初始化：检查是否为常量初始化
            ConstInt * constInt = dynamic_cast<ConstInt *>(initValueNode->val);
            if (constInt) {
                // 常量初始化，可以直接处理
                // 对于全局变量的常量初始化，在生成汇编代码时会处理
                // 这里我们标记该全局变量不在BSS段
                GlobalVariable * globalVar = dynamic_cast<GlobalVariable *>(varValue);
                if (globalVar) {
                    // 保存初始化值到GlobalVariable中
                    globalVar->setInitialValue(constInt);

                    // 如果初始化值不为0，则不在BSS段
                    if (constInt->getVal() != 0) {
                        globalVar->setInBSSSection(false);
                        minic_log(LOG_INFO,
                                  "全局变量 %s 使用非零常量(%d)初始化，放置在data段",
                                  varName.c_str(),
                                  constInt->getVal());
                    } else {
                        globalVar->setInBSSSection(true);
                        minic_log(LOG_INFO, "全局变量 %s 使用零常量初始化，放置在BSS段", varName.c_str());
                    }
                }
            } else {
                // 非常量初始化，目前不支持
                minic_log(LOG_ERROR, "全局变量 %s 不支持非常量初始化", varName.c_str());
                return false;
            }
        }
    } else {
        // minic_log(LOG_INFO, "变量 %s 没有初始化表达式", varName.c_str());
    }

    return true;
}

// IRGenerator.cpp 修改逻辑运算符处理
bool IRGenerator::ir_compare_op(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 处理逻辑运算符的特殊情况
    if (node->node_type == ast_operator_type::AST_OP_LOGICAL_OR ||
        node->node_type == ast_operator_type::AST_OP_LOGICAL_AND) {

        // 创建标签用于短路求值和结果合并
        LabelInstruction * evalRightLabel = new LabelInstruction(currentFunc);
        LabelInstruction * mergeLabel = new LabelInstruction(currentFunc);

        // 创建临时变量存储最终结果
        LocalVariable * resultVar = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeBool()));

        // 处理左操作数
        ast_node * left = ir_visit_ast_node(node->sons[0]);
        if (!left || !left->val)
            return false;
        node->blockInsts.addInst(left->blockInsts);

        // 确保左操作数是布尔类型
        Value * leftCondValue = left->val;
        if (!leftCondValue->getType()->isInt1Byte()) {
            BinaryInstruction * cmpInst = new BinaryInstruction(currentFunc,
                                                                IRInstOperator::IRINST_OP_NE_I,
                                                                leftCondValue,
                                                                module->newConstInt(0),
                                                                IntegerType::getTypeBool());
            node->blockInsts.addInst(cmpInst);
            leftCondValue = cmpInst;
        }

        // 根据运算符类型生成不同的短路逻辑
        if (node->node_type == ast_operator_type::AST_OP_LOGICAL_OR) {
            // 对于||，如果左操作数为真则短路（结果为真），否则计算右操作数
            // 先设置默认值为左操作数的值
            node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, leftCondValue));

            // 如果左操作数为假，才需要计算右操作数
            BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                                   IRInstOperator::IRINST_OP_BC,
                                                                   leftCondValue,
                                                                   mergeLabel,
                                                                   IntegerType::getTypeBool());
            branchInst->addOperand(evalRightLabel);
            node->blockInsts.addInst(branchInst);
        } else {
            // 对于&&，如果左操作数为假则短路（结果为假），否则计算右操作数
            // 先设置默认值为左操作数的值
            node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, leftCondValue));

            // 如果左操作数为真，才需要计算右操作数
            BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                                   IRInstOperator::IRINST_OP_BC,
                                                                   leftCondValue,
                                                                   evalRightLabel,
                                                                   IntegerType::getTypeBool());
            branchInst->addOperand(mergeLabel);
            node->blockInsts.addInst(branchInst);
        }

        // 计算右操作数
        node->blockInsts.addInst(evalRightLabel);
        ast_node * right = ir_visit_ast_node(node->sons[1]);
        if (!right || !right->val)
            return false;
        node->blockInsts.addInst(right->blockInsts);

        // 确保右操作数是布尔类型
        Value * rightCondValue = right->val;
        if (!rightCondValue->getType()->isInt1Byte()) {
            BinaryInstruction * cmpInst = new BinaryInstruction(currentFunc,
                                                                IRInstOperator::IRINST_OP_NE_I,
                                                                rightCondValue,
                                                                module->newConstInt(0),
                                                                IntegerType::getTypeBool());
            node->blockInsts.addInst(cmpInst);
            rightCondValue = cmpInst;
        }

        // 根据右操作数的值设置最终结果
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, rightCondValue));

        // 合并点
        node->blockInsts.addInst(mergeLabel);

        // 设置节点值为最终结果
        node->val = resultVar;
        return true;
    }

    // 处理普通比较运算符
    ast_node * left = ir_visit_ast_node(node->sons[0]);
    if (!left)
        return false;

    ast_node * right = ir_visit_ast_node(node->sons[1]);
    if (!right)
        return false;

    // 确保操作数已初始化
    if (!left->val || !right->val) {
        minic_log(LOG_ERROR, "比较运算符的操作数未初始化");
        return false;
    }

    IRInstOperator op;
    switch (node->node_type) {
        case ast_operator_type::AST_OP_GT:
            op = IRInstOperator::IRINST_OP_GT_I;
            break;
        case ast_operator_type::AST_OP_LT:
            op = IRInstOperator::IRINST_OP_LT_I;
            break;
        case ast_operator_type::AST_OP_GE:
            op = IRInstOperator::IRINST_OP_GE_I;
            break;
        case ast_operator_type::AST_OP_LE:
            op = IRInstOperator::IRINST_OP_LE_I;
            break;
        case ast_operator_type::AST_OP_EQ:
            op = IRInstOperator::IRINST_OP_EQ_I;
            break;
        case ast_operator_type::AST_OP_NE:
            op = IRInstOperator::IRINST_OP_NE_I;
            break;
        default:
            return false;
    }

    // 确保操作数类型一致
    if (left->val->getType() != right->val->getType()) {
        // 如果类型不匹配，尝试将右操作数转换为左操作数类型
        if (right->val->getType()->isIntegerType()) {
            LocalVariable * temp = static_cast<LocalVariable *>(module->newVarValue(left->val->getType()));
            node->blockInsts.addInst(new MoveInstruction(currentFunc, temp, right->val));
            right->val = temp;
        } else {
            minic_log(LOG_ERROR, "比较运算符操作数类型不匹配且无法自动转换");
            return false;
        }
    }

    // 添加指令，先处理左右操作数，再添加比较指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst =
        new BinaryInstruction(currentFunc, op, left->val, right->val, IntegerType::getTypeBool());
    node->blockInsts.addInst(cmpInst);

    // 设置节点值
    node->val = cmpInst;

    return true;
}

bool IRGenerator::ir_if(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "No current function in ir_if");
        return false;
    }

    // 创建基本块标签
    LabelInstruction * trueLabel = new LabelInstruction(currentFunc);
    LabelInstruction * falseLabel = new LabelInstruction(currentFunc);
    LabelInstruction * endLabel = new LabelInstruction(currentFunc);

    // 1. 处理条件表达式
    ast_node * cond_node = node->sons[0];
    ast_node * cond_result = ir_visit_ast_node(cond_node);
    if (!cond_result || !cond_result->val) {
        minic_log(LOG_ERROR, "Invalid condition expression in if statement");
        return false;
    }
    node->blockInsts.addInst(cond_result->blockInsts);

    // 确保条件是布尔类型
    Value * condValue = cond_result->val;
    // 只有当条件不是布尔类型时才创建比较指令
    // 比较操作符（如 >、<、==、!= 等）已经返回布尔类型，不需要额外转换

    // 调试：打印条件类型信息
    Type * condType = condValue->getType();
    printf("DEBUG: 条件类型检查 - isInt1Byte: %s\n", condType->isInt1Byte() ? "true" : "false");
    if (auto intType = dynamic_cast<IntegerType *>(condType)) {
        printf("DEBUG: 条件是整数类型，位宽: %d\n", intType->getBitWidth());
    }

    if (!condValue->getType()->isInt1Byte()) {
        printf("DEBUG: 创建额外的比较指令\n");
        // 创建比较指令将其转换为布尔类型
        BinaryInstruction * cmpInst = new BinaryInstruction(currentFunc,
                                                            IRInstOperator::IRINST_OP_NE_I,
                                                            condValue,
                                                            module->newConstInt(0),
                                                            IntegerType::getTypeBool());
        node->blockInsts.addInst(cmpInst);
        condValue = cmpInst;
    } else {
        printf("DEBUG: 条件已经是布尔类型，不需要额外转换\n");
    }

    // 2. 创建条件分支指令
    BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                           IRInstOperator::IRINST_OP_BC,
                                                           condValue,
                                                           trueLabel,
                                                           IntegerType::getTypeBool());
    branchInst->addOperand(falseLabel);
    node->blockInsts.addInst(branchInst);

    // 3. 处理true分支
    node->blockInsts.addInst(trueLabel);

    bool trueHasReturn = false;
    ast_node * true_block = node->sons[1];
    if (true_block) {
        // 处理true分支
        ast_node * true_result = ir_visit_ast_node(true_block);
        if (!true_result) {
            return false;
        }
        node->blockInsts.addInst(true_result->blockInsts);

        // 检查true分支是否已经有return语句或跳转到函数出口
        for (auto inst: true_result->blockInsts.getInsts()) {
            if (auto gotoInst = dynamic_cast<GotoInstruction *>(inst)) {
                if (gotoInst->getTarget() == currentFunc->getExitLabel()) {
                    trueHasReturn = true;
                    break;
                }
            }
        }
    }

    // 如果没有return语句，添加跳转到结束标签
    if (!trueHasReturn) {
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));
    }

    // 4. 处理false分支
    node->blockInsts.addInst(falseLabel);

    bool falseHasReturn = false;
    // 如果存在else分支
    if (node->sons.size() > 2 && node->sons[2]) {
        ast_node * false_block = node->sons[2];

        // 处理false分支
        ast_node * false_result = ir_visit_ast_node(false_block);
        if (!false_result) {
            return false;
        }
        node->blockInsts.addInst(false_result->blockInsts);

        // 检查false分支是否已经有return语句或跳转到函数出口
        for (auto inst: false_result->blockInsts.getInsts()) {
            if (auto gotoInst = dynamic_cast<GotoInstruction *>(inst)) {
                if (gotoInst->getTarget() == currentFunc->getExitLabel()) {
                    falseHasReturn = true;
                    break;
                }
            }
        }
    }

    // 如果没有return语句，且不是两个分支都有return，则需要添加结束标签
    if (!falseHasReturn || !trueHasReturn) {
        // 5. 添加结束标签
        node->blockInsts.addInst(endLabel);
    }

    return true;
}

/// @brief while循环AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_loop(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "No current function in ir_loop");
        return false;
    }

    // 1. 创建循环所需的标签
    LabelInstruction * condLabel = new LabelInstruction(currentFunc); // 条件判断标签
    LabelInstruction * bodyLabel = new LabelInstruction(currentFunc); // 循环体标签
    LabelInstruction * exitLabel = new LabelInstruction(currentFunc); // 循环结束标签

    // 将当前循环的退出标签和条件标签加入到循环上下文栈中，以便break和continue语句使用
    currentFunc->enterLoop(exitLabel, condLabel);

    // 2. 首先跳转到条件判断
    node->blockInsts.addInst(new GotoInstruction(currentFunc, condLabel));

    // 3. 添加条件判断标签
    node->blockInsts.addInst(condLabel);

    // 4. 处理循环条件表达式
    ast_node * cond_node = node->sons[0];
    ast_node * cond_result = ir_visit_ast_node(cond_node);
    if (!cond_result || !cond_result->val) {
        minic_log(LOG_ERROR, "Invalid condition expression in while statement");
        currentFunc->exitLoop(); // 确保在错误时也能正确退出循环上下文
        return false;
    }
    node->blockInsts.addInst(cond_result->blockInsts);

    // 确保条件是布尔类型
    Value * condValue = cond_result->val;
    if (!condValue) {
        minic_log(LOG_ERROR, "条件表达式计算结果为空");
        currentFunc->exitLoop();
        return false;
    }

    // 检查并转换条件值为布尔类型
    if (!condValue->getType() || !condValue->getType()->isInt1Byte()) {
        // 创建比较指令将其转换为布尔类型 (非0为真)
        BinaryInstruction * cmpInst = new BinaryInstruction(currentFunc,
                                                            IRInstOperator::IRINST_OP_NE_I,
                                                            condValue,
                                                            module->newConstInt(0),
                                                            IntegerType::getTypeBool());
        node->blockInsts.addInst(cmpInst);
        condValue = cmpInst;
    }

    // 5. 创建条件分支指令 - 根据条件值跳转到循环体或退出循环
    BinaryInstruction * branchInst = new BinaryInstruction(currentFunc,
                                                           IRInstOperator::IRINST_OP_BC,
                                                           condValue,
                                                           bodyLabel,
                                                           IntegerType::getTypeBool());
    branchInst->addOperand(exitLabel); // 条件为假时跳转到退出标签
    node->blockInsts.addInst(branchInst);

    // 6. 添加循环体标签
    node->blockInsts.addInst(bodyLabel);

    // 7. 处理循环体
    ast_node * body_node = node->sons[1];
    if (body_node) {
        ast_node * body_result = ir_visit_ast_node(body_node);
        if (!body_result) {
            minic_log(LOG_ERROR, "Failed to process while loop body");
            currentFunc->exitLoop(); // 确保在错误时也能正确退出循环上下文
            return false;
        }
        node->blockInsts.addInst(body_result->blockInsts);
    }

    // 8. 循环体执行完后跳回条件判断
    node->blockInsts.addInst(new GotoInstruction(currentFunc, condLabel));

    // 9. 添加循环退出标签
    node->blockInsts.addInst(exitLabel);

    // 循环处理完毕，退出循环上下文
    currentFunc->exitLoop();

    return true;
}

/// @brief break语句AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_break(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "No current function in ir_break");
        return false;
    }

    // 获取当前循环的退出标签
    Instruction * exitLabel = currentFunc->getCurrentLoopExitLabel();
    if (!exitLabel) {
        // 如果不在循环内部，报错
        minic_log(LOG_ERROR, "Break statement outside of loop");
        return false;
    }

    // 生成跳转指令，跳转到当前循环的退出标签
    node->blockInsts.addInst(new GotoInstruction(currentFunc, exitLabel));

    return true;
}

/// @brief continue语句AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_continue(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "No current function in ir_continue");
        return false;
    }

    // 获取当前循环的条件标签
    Instruction * condLabel = currentFunc->getCurrentLoopCondLabel();
    if (!condLabel) {
        // 如果不在循环内部，报错
        minic_log(LOG_ERROR, "Continue statement outside of loop");
        return false;
    }

    // 生成跳转指令，跳转到当前循环的条件标签
    node->blockInsts.addInst(new GotoInstruction(currentFunc, condLabel));

    return true;
}

/// @brief 数组访问AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_array_access(ast_node * node)
{
    // 参数检查 - 新的结构应该正好有2个子节点：ID和索引列表
    if (!node || node->sons.size() != 2) {
        minic_log(LOG_ERROR, "数组访问节点格式错误");
        return false;
    }

    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "数组访问必须在函数内部");
        return false;
    }

    // 第一个子节点是数组变量ID
    ast_node * arrayIdNode = node->sons[0];
    if (arrayIdNode->node_type != ast_operator_type::AST_OP_LEAF_VAR_ID) {
        minic_log(LOG_ERROR, "数组访问的第一个子节点必须是变量ID");
        return false;
    }

    std::string arrayName = arrayIdNode->name;

    // 第二个子节点应该是索引列表
    ast_node * indicesNode = node->sons[1];
    if (indicesNode->node_type != ast_operator_type::AST_OP_ARRAY_INDICES) {
        minic_log(LOG_ERROR, "数组访问的第二个子节点必须是索引列表");
        return false;
    }

    // 查找数组变量
    Value * arrayValue = module->findVarValue(arrayName);
    if (!arrayValue) {
        minic_log(LOG_ERROR, "未找到数组变量: %s", arrayName.c_str());
        return false;
    }

    // 处理索引表达式 - 从索引列表节点中获取所有索引
    std::vector<Value *> indices;
    for (auto indexExprNode: indicesNode->sons) {
        ast_node * indexExpr = ir_visit_ast_node(indexExprNode);
        if (!indexExpr || !indexExpr->val) {
            minic_log(LOG_ERROR, "数组索引表达式无效");
            return false;
        }

        // 添加索引表达式的指令
        node->blockInsts.addInst(indexExpr->blockInsts);
        indices.push_back(indexExpr->val);
    }

    // 获取数组维度信息
    std::vector<uint32_t> dimensions;

    LocalVariable * localArrayVar = dynamic_cast<LocalVariable *>(arrayValue);
    GlobalVariable * globalArrayVar = dynamic_cast<GlobalVariable *>(arrayValue);

    if (localArrayVar && localArrayVar->isArray()) {
        dimensions = localArrayVar->getArrayDimensions();
    } else if (globalArrayVar && globalArrayVar->isArray()) {
        dimensions = globalArrayVar->getArrayDimensions();
    } else {
        minic_log(LOG_ERROR, "变量 %s 不是数组", arrayName.c_str());
        return false;
    }

    // 计算数组元素的偏移地址
    Value * offsetValue = nullptr;

    if (indices.size() == 1) {
        // 一维数组：offset = index * elementSize（这里elementSize假设为1）
        offsetValue = indices[0];
    } else {
        // 多维数组：使用递归公式计算偏移
        // offset = ((i0 * d1 + i1) * d2 + i2) * d3 + ... + iN
        offsetValue = indices[0];

        for (size_t i = 1; i < indices.size(); i++) {
            // offset = offset * dimensions[i] + indices[i]
            Instruction * mulInst = new BinaryInstruction(currentFunc,
                                                          IRInstOperator::IRINST_OP_MUL_I,
                                                          offsetValue,
                                                          new ConstInt(dimensions[i]),
                                                          IntegerType::getTypeInt());
            node->blockInsts.addInst(mulInst);

            Instruction * addInst = new BinaryInstruction(currentFunc,
                                                          IRInstOperator::IRINST_OP_ADD_I,
                                                          mulInst,
                                                          indices[i],
                                                          IntegerType::getTypeInt());
            node->blockInsts.addInst(addInst);

            offsetValue = addInst;
        }
    }

    // 将偏移量乘以元素大小（sizeof(int) = 4）
    Instruction * sizeOffsetInst = new BinaryInstruction(currentFunc,
                                                         IRInstOperator::IRINST_OP_MUL_I,
                                                         offsetValue,
                                                         new ConstInt(4),
                                                         IntegerType::getTypeInt());
    node->blockInsts.addInst(sizeOffsetInst);

    // 对于数组访问，直接使用数组名进行指针算术运算
    // 全局数组名和局部数组名都可以直接用作指针算术的基地址

    // 创建一个指针类型的临时变量来表示数组元素的地址
    const PointerType * ptrType = PointerType::get(IntegerType::getTypeInt());
    Instruction * addrInst = new BinaryInstruction(currentFunc,
                                                   IRInstOperator::IRINST_OP_ADD_I,
                                                   arrayValue,
                                                   sizeOffsetInst,
                                                   const_cast<Type *>(static_cast<const Type *>(ptrType)));
    node->blockInsts.addInst(addrInst);

    // 检查是否需要生成加载指令（用于右值情况）
    // 如果数组访问不是赋值语句的左操作数，则可能需要生成加载指令
    bool isLValue = false;

    // 遍历父节点，检查是否在赋值语句的左侧
    ast_node * parent = node->parent;
    while (parent) {
        if (parent->node_type == ast_operator_type::AST_OP_ASSIGN) {
            // 如果当前节点是赋值节点的第一个子节点（左操作数），则是左值
            if (parent->sons.size() > 0 && parent->sons[0] == node) {
                isLValue = true;
            }
            break;
        }
        parent = parent->parent;
    }

    if (isLValue) {
        // 左值：直接使用地址
        node->val = addrInst;
    } else {
        // 右值情况：需要判断是否完全访问所有维度
        if (indices.size() < dimensions.size()) {
            // 部分访问：结果是指向子数组的指针，不需要解引用
            // 为地址指令设置剩余维度信息
            std::vector<uint32_t> remainingDims(dimensions.begin() + indices.size(), dimensions.end());
            if (auto tempVar = dynamic_cast<Value *>(addrInst)) {
                // 设置临时变量的数组维度信息
                if (auto localVar = dynamic_cast<LocalVariable *>(tempVar)) {
                    localVar->setArrayDimensions(remainingDims);
                }
            }
            node->val = addrInst;
        } else {
            // 完全访问：需要解引用
            Value * tempVar = module->newVarValue(IntegerType::getTypeInt());
            Instruction * loadInst = new MoveInstruction(currentFunc, tempVar, addrInst);
            node->blockInsts.addInst(loadInst);
            node->val = tempVar;
        }
    }

    minic_log(LOG_INFO,
              "处理数组访问: %s[%zu个索引], isLValue=%s",
              arrayName.c_str(),
              indices.size(),
              isLValue ? "true" : "false");

    return true;
}
