///
/// @file BinaryInstruction.h
/// @brief 二元操作指令，如加和减
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
#pragma once

#include "Instruction.h"

///
/// @brief 二元运算指令
///
class BinaryInstruction : public Instruction {

public:
    /// @brief 构造函数(二元指令)
    BinaryInstruction(Function * _func, IRInstOperator _op, Value * _srcVal1, Value * _srcVal2, Type * _type);

    /// @brief 构造函数(三元指令，主要用于bc指令)
    BinaryInstruction(Function * _func,
                      IRInstOperator _op,
                      Value * _srcVal1,
                      Value * _srcVal2,
                      Value * _srcVal3,
                      Type * _type);

    /// @brief 转换成字符串
    void toString(std::string & str) override;
};