package com.anwen.mongo.conditions.interfaces.aggregate;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.enums.GroupTypeEnum;
import com.anwen.mongo.support.SFunction;

import java.io.Serializable;
import java.util.List;

/**
 * 管道操作符
 *
 * @author JiaChaoYang
 **/
public interface Aggregate<T,Children> extends Serializable {

    /**
     * 过滤文档记录，只将匹配的文档记录传递到管道中的下一个步骤
     * @param queryChainWrapper 条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:03
    */
    Children match(QueryChainWrapper<T, ?> queryChainWrapper);

    /**
     * 控制哪些字段返回，如_id:0,name:1 返回name字段值，过滤_id字段值
     * @author JiaChaoYang
     * @date 2023/8/12 21:04
    */
    Children project(Projection... projection);

    /**
     * 对所有输出的文档记录进行排序
     * @param orders 排序对象
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
    */
    Children sort(Order... orders);

    /**
     * 限制管道中文档记录的数量(每页显示行数)
     * @param l 数量
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
    */
    Children limit(long l);

    /**
     * 跳过指定数量的文档记录，返回剩下的文档记录（当前页）
     * @param s 数量
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:06
    */
    Children skip(long s);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param
     * @param _id
     * @param accumulator
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(SFunction<T,Object> _id,Accumulator accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param
     * @param _id
     * @param accumulator
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(String _id,Accumulator accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param
     * @param accumulator
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(SFunction<T,Object> _id , Accumulator... accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param
     * @param _id
     * @param accumulator
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(String _id , Accumulator... accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param
     * @param accumulatorList
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(List<Accumulator> accumulatorList);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param resultMappingField 结果映射字段
     * @param operator 操作，参考{@link com.anwen.mongo.enums.GroupTypeEnum}枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
    */
    Children group(String resultMappingField,String operator,String field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param resultMappingField 结果映射字段
     * @param operator Group条件枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(String resultMappingField, GroupTypeEnum operator, String field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param resultMappingField 结果映射字段
     * @param operator 操作，参考{@link com.anwen.mongo.enums.GroupTypeEnum}枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(SFunction<T,Object> resultMappingField, String operator, SFunction<T,Object> field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param resultMappingField 结果映射字段
     * @param operator Group条件枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(SFunction<T,Object> resultMappingField, GroupTypeEnum operator, SFunction<T,Object> field);

    /**
     * 实现集合之间的join操作
     * @param
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
    */
    Children lookup();

    /**
     * 向集合中添加新字段
     * @param
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:08
    */
    Children addFields();

    /**
     * 将管道中的文档记录输出到一个具体的集合中，这个必须是管道操作中的最后一步
     * @param
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:08
    */
    Children out();
}
