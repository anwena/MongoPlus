package com.anwen.mongo.conditions.accumulator;

import com.anwen.mongo.enums.GroupTypeEnum;
import com.anwen.mongo.support.SFunction;

/**
 * 构建累加器
 *
 * @author JiaChaoYang
 **/
public class AccumulatorInterface<T> {

    /**
     * 获取指定字段在分组中的第一个文档的值
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:15
    */
    public Accumulator first(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.FIRST.getOperator(),field.getFieldNameLine());
    }

    /**
     * 获取指定字段在分组中的第一个文档的值，并指定返回字段
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:15
     */
    public Accumulator first(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.FIRST.getOperator(),field.getFieldNameLine());
    }

    /**
     * 获取指定字段在分组中的第一个文档的值，并指定返回字段
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:15
     */
    public Accumulator first(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.FIRST.getOperator(),field.getFieldNameLine());
    }

    /**
     * 获取指定字段在分组中的最后一个文档的值
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
    */
    public Accumulator last(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.LAST.getOperator(), field.getFieldNameLine());
    }

    /**
     * 获取指定字段在分组中的最后一个文档的值，并指定返回字段
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
     */
    public Accumulator last(SFunction<T,Object> resultMappingField , SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.LAST.getOperator(), field.getFieldNameLine());
    }

    /**
     * 获取指定字段在分组中的最后一个文档的值，并指定返回字段
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
     */
    public Accumulator last(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.LAST.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段的总和
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
    */
    public Accumulator sum(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.SUM.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段的总和
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
     */
    public Accumulator sum(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.SUM.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段的总和
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
     */
    public Accumulator sum(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.SUM.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段的平均值
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
    */
    public Accumulator avg(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.AVG.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段的平均值
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
     */
    public Accumulator avg(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.AVG.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段的平均值
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:16
     */
    public Accumulator avg(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.AVG.getOperator(), field.getFieldNameLine());
    }

    /**
     * 查找指定字段的最大值
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:17
    */
    public Accumulator max(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.MAX.getOperator(), field.getFieldNameLine());
    }

    /**
     * 查找指定字段的最大值
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:17
     */
    public Accumulator max(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.MAX.getOperator(), field.getFieldNameLine());
    }

    /**
     * 查找指定字段的最大值
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:17
     */
    public Accumulator max(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.MAX.getOperator(), field.getFieldNameLine());
    }

    /**
     * 查找指定字段的最小值
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
    */
    public Accumulator min(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.MIN.getOperator(), field.getFieldNameLine());
    }

    /**
     * 查找指定字段的最小值
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator min(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.MIN.getOperator(), field.getFieldNameLine());
    }

    /**
     * 查找指定字段的最小值
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator min(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.MIN.getOperator(), field.getFieldNameLine());
    }

    /**
     * 将指定字段的值添加到数组中
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
    */
    public Accumulator push(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.PUSH.getOperator(), field.getFieldNameLine());
    }

    /**
     * 将指定字段的值添加到数组中
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator push(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.PUSH.getOperator(), field.getFieldNameLine());
    }

    /**
     * 将指定字段的值添加到数组中
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator push(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.PUSH.getOperator(), field.getFieldNameLine());
    }

    /**
     * 将指定字段的唯一值添加到数组中
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
    */
    public Accumulator addToSet(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.ADD_TO_SET.getOperator(), field.getFieldNameLine());
    }

    /**
     * 将指定字段的唯一值添加到数组中
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator addToSet(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.ADD_TO_SET.getOperator(), field.getFieldNameLine());
    }

    /**
     * 将指定字段的唯一值添加到数组中
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator addToSet(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.ADD_TO_SET.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段非空值的个数
     * <p style='color: red;'>默认返回字段会使用field参数传递的字段</p>
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
    */
    public Accumulator count(SFunction<T,Object> field){
        return new Accumulator(GroupTypeEnum.COUNT.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段非空值的个数
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator count(SFunction<T,Object> resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField.getFieldNameLine(),GroupTypeEnum.COUNT.getOperator(), field.getFieldNameLine());
    }

    /**
     * 计算指定字段非空值的个数
     * @param resultMappingField 返回字段
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:18
     */
    public Accumulator count(String resultMappingField,SFunction<T,Object> field){
        return new Accumulator(resultMappingField,GroupTypeEnum.COUNT.getOperator(), field.getFieldNameLine());
    }

}
