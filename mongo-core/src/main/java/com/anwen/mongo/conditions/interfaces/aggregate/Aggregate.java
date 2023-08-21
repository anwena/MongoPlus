package com.anwen.mongo.conditions.interfaces.aggregate;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.Project;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.AddFields;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Let;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.ReplaceRoot;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.enums.GroupTypeEnum;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * 管道操作符
 *
 * @author JiaChaoYang
 **/
public interface Aggregate<T,Children> extends Project<T,Children> {

    /**
     * 过滤文档记录，只将匹配的文档记录传递到管道中的下一个步骤
     * @param queryChainWrapper 条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:03
    */
    Children match(QueryChainWrapper<T, ?> queryChainWrapper);

    /**
     * 过滤文档记录，只将匹配的文档记录传递到管道中的下一个步骤
     * @param basicDBObject 条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:03
     */
    Children match(BasicDBObject basicDBObject);

    /**
     * 过滤文档记录，只将匹配的文档记录传递到管道中的下一个步骤
     * @param bson 条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:03
     */
    Children match(Bson bson);

    /**
     * 对所有输出的文档记录进行排序
     * @param orders 排序对象
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
    */
    Children sort(Order... orders);

    /**
     * 对所有输出的文档记录进行排序
     * @param orderList 排序对象集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
     */
    Children sort(List<Order> orderList);

    /**
     * 正序排序
     * @param field 排序字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/19 22:28
    */
    Children sortAsc(SFunction<T,Object>... field);

    /**
     * 正序排序
     * @param field 排序字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/19 22:28
    */
    Children sortAsc(String... field);

    /**
     * 倒序排序
     * @param field 排序字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/19 22:30
    */
    Children sortDesc(SFunction<T,Object>... field);

    /**
     * 倒序排序
     * @param field 排序字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/19 22:30
    */
    Children sortDesc(String... field);

    /**
     * 对所有输出的文档记录进行排序
     * @param basicDBObject 排序条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
     */
    Children sort(BasicDBObject basicDBObject);

    /**
     * 对所有输出的文档记录进行排序
     * @param bson 排序条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
     */
    Children sort(Bson bson);

    /**
     * 限制管道中文档记录的数量(每页显示行数)
     * @param limit 数量
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:05
    */
    Children limit(long limit);

    /**
     * 跳过指定数量的文档记录，返回剩下的文档记录（当前页）
     * @param skip 数量
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:06
    */
    Children skip(long skip);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(SFunction<T,Object> _id);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(String _id);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * 这种情况最好使用map，因为返回值会是一个json，如：{"name": "超级管理员", "age": 100}
     * @param _id 累加器类
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:00
    */
    Children group(Accumulator... _id);
    
    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param _id 分组依据字段
     * @param accumulator 累加器类
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(SFunction<T,Object> _id,Accumulator accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     *
     * @param _id 分组依据字段
     * @param accumulator 累加器类
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(String _id,Accumulator accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param accumulator 累加器类
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(SFunction<T,Object> _id , Accumulator... accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param accumulator 累加器类
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(String _id , Accumulator... accumulator);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param accumulatorList 累加器集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(SFunction<T,Object> _id , List<Accumulator> accumulatorList);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param accumulatorList 累加器集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(String _id , List<Accumulator> accumulatorList);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator 操作，参考{@link com.anwen.mongo.enums.GroupTypeEnum}枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
    */
    Children group(SFunction<T,Object> _id , String resultMappingField,String operator,String field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator 操作，参考{@link com.anwen.mongo.enums.GroupTypeEnum}枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(String _id , String resultMappingField,String operator,String field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator           Group条件枚举
     * @param field              列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(SFunction<T,Object> _id , String resultMappingField, GroupTypeEnum operator, String field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator Group条件枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(String _id , String resultMappingField, GroupTypeEnum operator, String field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator 操作，参考{@link com.anwen.mongo.enums.GroupTypeEnum}枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(SFunction<T,Object> _id , SFunction<T,Object> resultMappingField, String operator, SFunction<T,Object> field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator 操作，参考{@link com.anwen.mongo.enums.GroupTypeEnum}枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(String _id , SFunction<T,Object> resultMappingField, String operator, SFunction<T,Object> field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator Group条件枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(String _id , SFunction<T,Object> resultMappingField, GroupTypeEnum operator, SFunction<T,Object> field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param _id 分组依据字段
     * @param resultMappingField 结果映射字段
     * @param operator Group条件枚举
     * @param field 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/17 22:51
     */
    Children group(SFunction<T,Object> _id , SFunction<T,Object> resultMappingField, GroupTypeEnum operator, SFunction<T,Object> field);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param basicDBObject 分组依据
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(BasicDBObject basicDBObject);

    /**
     * 对所有文档记录进行分组，然后计算聚合结果
     * @param bson 分组依据
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children group(Bson bson);

    /**
     * 实现集合之间的join操作
     *
     * @param from 目标集合名称
     * @param localField 当前集合用于关联的字段
     * @param foreignField 指定目标集合用于关联的字段
     * @param as 输出结果中保存关联值的字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children lookup(String from,String localField,String foreignField,String as);

    Children lookup(String from, List<Let> letList, AggregateChainWrapper<T,?> pipeline, String as);

    /**
     * 向集合中添加新字段
     * @param resultMappingField 结果映射字段
     * @param field 字段值、取值字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:13
    */
    Children addFields(String resultMappingField,SFunction<T,Object> field);

    /**
     * 向集合中添加新字段
     * @param resultMappingField 结果映射字段
     * @param field 字段值、取值字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:13
     */
    Children addFields(SFunction<T,Object> resultMappingField,SFunction<T,Object> field);

    /**
     * 向集合中添加新字段
     * @param resultMappingField 结果映射字段
     * @param field 字段值、取值字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:13
     */
    Children addFields(SFunction<T,Object> resultMappingField,String field);

    /**
     * 向集合中添加新字段
     * @param resultMappingField 结果映射字段
     * @param field 字段值、取值字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:13
     */
    Children addFields(String resultMappingField,String field);

    /**
     * 向集合中添加新字段
     * @param addFields 新字段类
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:15
    */
    Children addFields(AddFields... addFields);

    /**
     * 向集合中添加新字段
     * @param addFieldsList 新字段类集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:15
     */
    Children addFields(List<AddFields> addFieldsList);

    /**
     * 向集合中添加新字段
     * @param basicDBObject 自定义
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:15
     */
    Children addFields(BasicDBObject basicDBObject);

    /**
     * 向集合中添加新字段
     * @param bson 自定义
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:15
     */
    Children addFields(Bson bson);

    /**
     * 展开数组字段，生成一个文档副本，每个副本只包含一个数组元素
     * @param field 需要展开的字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:56
     */
    Children unwind(SFunction<T, Object> field);
    
    /**
     * 展开数组字段，生成一个文档副本，每个副本只包含一个数组元素
     * @param preserveNullAndEmptyArrays 是否保留空数组或包含 null 值的数组字段
     * @param field 需要展开的字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:56
    */
    Children unwind(Boolean preserveNullAndEmptyArrays , SFunction<T, Object> field);

    /**
     * 展开数组字段，生成一个文档副本，每个副本只包含一个数组元素
     * @param field 需要展开的字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:56
     */
    Children unwind(String field);

    /**
     * 展开数组字段，生成一个文档副本，每个副本只包含一个数组元素
     * @param preserveNullAndEmptyArrays 是否保留空数组或包含 null 值的数组字段
     * @param field 需要展开的字段
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:56
     */
    Children unwind(Boolean preserveNullAndEmptyArrays,String field);

    /**
     * 展开数组字段，生成一个文档副本，每个副本只包含一个数组元素
     * @param basicDBObject 自定义
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:56
     */
    Children unwind(BasicDBObject basicDBObject);

    /**
     * 展开数组字段，生成一个文档副本，每个副本只包含一个数组元素
     * @param bson 自定义
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/20 0:56
     */
    Children unwind(Bson bson);

    /**
     * 随机选择指定数量的文档
     * @param size 数量
     * @author JiaChaoYang
     * @date 2023/8/20 0:38
    */
    Children sample(long size);

    /**
     * 使用指定字段替换根文档
     * @param field 指定字段
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(SFunction<T,Object>... field);

    /**
     * 使用指定字段替换根文档
     * @param replaceRoot replaceRoot类，会使用mergeObjects操作符将值组合起来
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(ReplaceRoot... replaceRoot);

    /**
     * 使用指定字段替换根文档
     * @param replaceRootList replaceRoot集合，会使用mergeObjects操作符将值组合起来
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(List<ReplaceRoot> replaceRootList);

    /**
     * 使用指定字段替换根文档
     * @param field 指定字段，传递多个值时会使用mergeObjects操作符组合起来
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(String... field);

    /**
     * 使用指定字段替换根文档
     * @param reserveOriginalDocument 是否保留原始文档字段
     * @param field 指定字段，传递多个值时会使用mergeObjects操作符组合起来
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(Boolean reserveOriginalDocument,SFunction<T,Object>... field);

    /**
     * 使用指定字段替换根文档
     * @param reserveOriginalDocument 是否保留原始文档字段
     * @param field 指定字段，传递多个值时会使用mergeObjects操作符组合起来
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(Boolean reserveOriginalDocument,String... field);

    /**
     * 使用指定字段替换根文档
     * @param basicDBObject 自定义
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(BasicDBObject basicDBObject);

    /**
     * 使用指定字段替换根文档
     * @param bson 自定义
     * @author JiaChaoYang
     * @date 2023/8/20 0:37
     */
    Children replaceRoot(Bson bson);

    /**
     * unionAll
     * @param collectionName 集合名
     * @author JiaChaoYang
     * @date 2023/8/20 20:16
    */
    Children unionWith(String collectionName);

    /**
     * unionAll
     * @param basicDBObject 自定义
     * @author JiaChaoYang
     * @date 2023/8/20 20:16
     */
    Children unionWith(BasicDBObject basicDBObject);

    /**
     * unionAll
     * @param bson 自定义
     * @author JiaChaoYang
     * @date 2023/8/20 20:16
     */
    Children unionWith(Bson bson);

    /**
     * 将管道中的文档记录输出到一个具体的集合中，不存在则自动创建
     * <p style='color:red'>这个必须是管道操作中的最后一步，如果输出到现有集合，会覆盖原数据</p>
     * @param coll 集合名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:08
    */
    @Deprecated
    Children out(String coll);

    /**
     * 将管道中的文档记录输出到一个具体的集合中，不存在则自动创建
     * <p style='color:red'>这个必须是管道操作中的最后一步，如果输出到现有集合，会覆盖原数据</p>
     * @param db 库
     * @param coll 集合名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:08
     */
    Children out(String db,String coll);

    /**
     * 将管道中的文档记录输出到一个具体的集合中，不存在则自动创建
     * <p style='color:red'>这个必须是管道操作中的最后一步，如果输出到现有集合，会覆盖原数据</p>
     * @param basicDBObject 自定义
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:08
     */
    Children out(BasicDBObject basicDBObject);

    /**
     * 将管道中的文档记录输出到一个具体的集合中，不存在则自动创建
     * <p style='color:red'>这个必须是管道操作中的最后一步，如果输出到现有集合，会覆盖原数据</p>
     * @param bson 自定义
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:08
     */
    Children out(Bson bson);

    /**
     * 自定义管道操作
     * @author JiaChaoYang
     * @date 2023/8/20 20:46
    */
    Children custom(BasicDBObject basicDBObject);

    /**
     * 自定义管道操作
     * @author JiaChaoYang
     * @date 2023/8/20 20:46
    */
    Children custom(Bson bson);

}
