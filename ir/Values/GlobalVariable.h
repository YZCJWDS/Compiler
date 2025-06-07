///
/// @file GlobalVariable.h
/// @brief 全局变量描述类
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

#include "GlobalValue.h"
#include "IRConstant.h"
#include "ConstInt.h"

// 前向声明
class Constant;

///
/// @brief 全局变量，寻址时通过符号名或变量名来寻址
///
class GlobalVariable : public GlobalValue {

public:
    ///
    /// @brief 构建全局变量，默认对齐为4字节
    /// @param _type 类型
    /// @param _name 名字
    ///
    explicit GlobalVariable(Type * _type, std::string _name) : GlobalValue(_type, _name)
    {
        // 设置对齐大小
        setAlignment(4);
    }

    ///
    /// @brief  检查是否是函数
    /// @return true 是函数
    /// @return false 不是函数
    ///
    [[nodiscard]] bool isGlobalVarible() const override
    {
        return true;
    }

    ///
    /// @brief 是否属于BSS段的变量，即未初始化过的变量，或者初值都为0的变量
    /// @return true
    /// @return false
    ///
    [[nodiscard]] bool isInBSSSection() const
    {
        return this->inBSSSection;
    }

    ///
    /// @brief 设置是否在BSS段
    /// @param inBSS 是否在BSS段
    ///
    void setInBSSSection(bool inBSS)
    {
        this->inBSSSection = inBSS;
    }

    ///
    /// @brief 设置初始化值
    /// @param initVal 初始化值
    ///
    void setInitialValue(Constant * initVal)
    {
        this->initialValue = initVal;
    }

    ///
    /// @brief 获取初始化值
    /// @return Constant* 初始化值，如果没有则返回nullptr
    ///
    [[nodiscard]] Constant * getInitialValue() const
    {
        return this->initialValue;
    }

    ///
    /// @brief 取得变量所在的作用域层级
    /// @return int32_t 层级
    ///
    int32_t getScopeLevel() override
    {
        return 0;
    }

    ///
    /// @brief 对该Value进行Load用的寄存器编号
    /// @return int32_t 寄存器编号
    ///
    int32_t getLoadRegId() override
    {
        return this->loadRegNo;
    }

    ///
    /// @brief 对该Value进行Load用的寄存器编号
    /// @return int32_t 寄存器编号
    ///
    void setLoadRegId(int32_t regId) override
    {
        this->loadRegNo = regId;
    }

    ///
    /// @brief Declare指令IR显示
    /// @param str
    ///
    void toDeclareString(std::string & str)
    {
        std::string typeStr = getType()->toString();
        std::string variableName = getIRName();

        // 如果是数组，需要将维度信息添加到变量名后面
        if (isArray()) {
            for (uint32_t dimension: arrayDimensions) {
                variableName += "[" + std::to_string(dimension) + "]";
            }
        }

        if (initialValue) {
            // 有初始值的全局变量，生成声明格式
            ConstInt * constIntVal = dynamic_cast<ConstInt *>(initialValue);
            if (constIntVal) {
                str = "declare " + typeStr + " " + variableName + " = " + std::to_string(constIntVal->getVal());
            } else {
                // 其他类型的常量初始值（目前只支持整数）
                str = "declare " + typeStr + " " + variableName + " = 0";
            }
        } else {
            // 无初始值的全局变量，生成声明格式
            str = "declare " + typeStr + " " + variableName;
        }
    }

private:
    ///
    /// @brief 变量加载到寄存器中时对应的寄存器编号
    ///
    int32_t loadRegNo = -1;

    ///
    /// @brief 默认全局变量在BSS段，没有初始化，或者即使初始化过，但都值都为0
    ///
    bool inBSSSection = true;

    ///
    /// @brief 全局变量的初始化值
    ///
    Constant * initialValue = nullptr;

    ///
    /// @brief 数组维度信息，空表示不是数组
    ///
    std::vector<uint32_t> arrayDimensions;

public:
    ///
    /// @brief 设置数组维度信息
    /// @param dimensions 维度数组
    ///
    void setArrayDimensions(const std::vector<uint32_t> & dimensions)
    {
        arrayDimensions = dimensions;
    }

    ///
    /// @brief 获取数组维度信息
    /// @return 维度数组
    ///
    [[nodiscard]] const std::vector<uint32_t> & getArrayDimensions() const
    {
        return arrayDimensions;
    }

    ///
    /// @brief 是否为数组变量
    /// @return true 是数组
    ///
    [[nodiscard]] bool isArray() const
    {
        return !arrayDimensions.empty();
    }
};
