///
/// @file MoveInstruction.cpp
/// @brief Move指令，也就是DragonIR的Asssign指令
///
/// @author zenglj (zenglj@live.com)
/// @version 1.0
/// @date 2024-09-29
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-09-29 <td>1.0     <td>zenglj  <td>新建
/// </table>
///

#include "VoidType.h"
#include "MoveInstruction.h"
#include "Values/FormalParam.h"

///
/// @brief 构造函数
/// @param _func 所属的函数
/// @param result 结构操作数
/// @param srcVal1 源操作数
///
MoveInstruction::MoveInstruction(Function * _func, Value * _result, Value * _srcVal1)
    : Instruction(_func, IRInstOperator::IRINST_OP_ASSIGN, VoidType::getType())
{
    addOperand(_result);
    addOperand(_srcVal1);
}

/// @brief 转换成字符串显示
/// @param str 转换后的字符串
void MoveInstruction::toString(std::string & str)
{
    Value *dstVal = getOperand(0), *srcVal = getOperand(1);

    // 检查是否是函数形参（数组参数）
    bool isSrcFormalParam = dynamic_cast<FormalParam *>(srcVal) != nullptr;
    bool isDstFormalParam = dynamic_cast<FormalParam *>(dstVal) != nullptr;

    // 函数参数的特殊处理，不使用指针解引用
    if (isDstFormalParam && dstVal->getType()->isPointerType()) {
        // 函数形参作为目标操作数，直接赋值不需要解引用
        str = dstVal->getIRName() + " = " + srcVal->getIRName();
    } else if (isSrcFormalParam && srcVal->getType()->isPointerType()) {
        // 函数形参作为源操作数，直接赋值不需要解引用
        str = dstVal->getIRName() + " = " + srcVal->getIRName();
    } else if (dstVal->getType()->isPointerType()) {
        // 普通指针作为目标，需要解引用
        str = "*" + dstVal->getIRName() + " = " + srcVal->getIRName();
    } else if (srcVal->getType()->isPointerType()) {
        // 普通指针作为源，需要解引用
        str = dstVal->getIRName() + " = *" + srcVal->getIRName();
    } else {
        // 普通变量赋值
        str = dstVal->getIRName() + " = " + srcVal->getIRName();
    }
}
