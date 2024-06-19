package com.anwen.mongo.aggregate;

import com.mongodb.client.model.CollationStrength;
import org.bson.BsonValue;
import org.bson.conversions.Bson;

/**
 * @author anwen
 * @date 2024/6/19 下午11:41
 */
public interface AggregateOptions<Children> {

    /**
     * 设置为 true，表示允许在磁盘上存储临时数据。默认值为 false。
     * @author JiaChaoYang
     * @date 2023/8/31 0:49
     */
    Children allowDiskUse(boolean allowDiskUse);

    /**
     * 设置每个批次大小
     * @param size 大小
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/31 0:49
     */
    Children batchSize(Integer size);

    /**
     * 用于指定排序规则、语言特性等。使用CollationStrength枚举设置
     * @param collationStrength 枚举
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/31 0:56
     */
    Children collation(CollationStrength collationStrength);

    /**
     * 设置查询的最大执行时间（以毫秒为单位）。超过设定的时间限制将导致查询被中断。默认值为无限制。
     * @param maxTime 执行时间
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/31 0:55
     */
    Children maxTimeMS(long maxTime);

    /**
     * 服务器等待新文档以满足$changeStream聚合的最长时间。
     * @param maxAwaitTime 最大等待时间
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:53
     */
    Children maxAwaitTimeMS(long maxAwaitTime);

    /**
     * 设置绕过文档级验证标志。
     * 注意：这仅适用于指定$out或$merge阶段的情况
     * @param bypassDocumentValidation 如果为true，则允许写入选择退出文档级验证。
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:55
     */
    Children bypassDocumentValidation(boolean bypassDocumentValidation);

    /**
     * 设置此操作的注释。空值表示未设置注释。
     * @param comment 注释
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:56
     */
    Children comment(BsonValue comment);

    /**
     * 设置此操作的注释。空值表示未设置注释。
     * @param comment 注释
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:56
     */
    Children comment(String comment);

    /**
     * 设置要使用的索引的提示。空值表示未设置提示。
     * @param hint 提示
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:58
     */
    Children hint(Bson hint);

    /**
     * 设置要使用的索引的提示。空值表示未设置提示。
     * @param hint 提示
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:58
     */
    Children hint(String hint);

    /**
     * 将顶级变量添加到聚合中。
     * 对于MongoDB 5.0+，聚合命令接受let选项。
     * 此选项是一个由零个或多个字段组成的文档，这些字段表示聚合管道可访问的变量。键是变量的名称，值是聚合表达式语言中的常量。
     * 然后，每个参数名称都可用于在聚合表达式上下文中使用“$$”语法访问相应表达式的值，聚合表达式上下文可能需要使用$expr或管道。
     * @param variables 变量
     * @return Children
     * @author JiaChaoYang
     * @date 2023/9/4 21:59
     */
    Children let(Bson variables);

}
