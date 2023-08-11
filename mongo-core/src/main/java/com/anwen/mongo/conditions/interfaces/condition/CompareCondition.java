package com.anwen.mongo.conditions.interfaces.condition;

import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.support.SFunction;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 构建条件对象
 * @author JiaChaoYang
 * @since 2023/2/14 14:13
*/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CompareCondition {

    /**
     * 条件
     * @since 2023/2/10 10:16
     */
    private String condition;

    /**
     * 字段
     * @since 2023/2/10 10:16
    */
    private String column;

    /**
     * 值
     * @since 2023/2/10 10:16
    */
    private Object value;

    /**
     * 类型 0查询，1修改
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:49
    */
    private Integer type;

    /**
     * 逻辑运算符类型 0 and 1 or
     * @author JiaChaoYang
     * @date 2023/7/16 19:07
    */
    private Integer logicType;

    /**
     * 子条件
     * @author JiaChaoYang
     * @date 2023/7/16 19:22
    */
    private List<CompareCondition> childCondition;

    /**
     * 构建条件
     * @param condition 条件，参考{@link QueryOperatorEnum}
     * @param column 列名、字段名
     * @param value 值
     * @return com.anwen.mongo.sql.interfaces.CompareCondition
     * @author JiaChaoYang
     * @date 2023/7/30 0:33
    */
    public static CompareCondition build(String condition,String column,Object value){
        return CompareCondition.builder().condition(condition).column(column).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build();
    }

    /**
     * 构建条件
     * @param condition 条件，参考{@link QueryOperatorEnum}
     * @param column 列名、字段名
     * @param value 值
     * @return com.anwen.mongo.sql.interfaces.CompareCondition
     * @author JiaChaoYang
     * @date 2023/7/30 0:33
     */
    public static <T> CompareCondition build(String condition, SFunction<T,Object> column, Object value){
        return CompareCondition.builder().condition(condition).column(column.getFieldNameLine()).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build();
    }

    /**
     * 构建条件
     * @param condition 查询条件枚举
     * @param column 列名、字段名
     * @param value 值
     * @return com.anwen.mongo.sql.interfaces.CompareCondition
     * @author JiaChaoYang
     * @date 2023/7/30 0:33
     */
    public static <T> CompareCondition build(QueryOperatorEnum condition, SFunction<T,Object> column, Object value){
        return CompareCondition.builder().condition(condition.getValue()).column(column.getFieldNameLine()).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build();
    }

    /**
     * 构建OR条件
     * @author JiaChaoYang
     * @date 2023/7/29 23:11
    */
    public static CompareCondition buildOr(List<CompareCondition> compareConditionList){
        return CompareCondition.builder().type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.OR.getKey()).childCondition(compareConditionList).build();
    }

}
