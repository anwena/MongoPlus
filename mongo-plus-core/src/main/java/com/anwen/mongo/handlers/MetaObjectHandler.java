package com.anwen.mongo.handlers;

import org.bson.Document;

/**
 * 元对象字段填充控制器抽象类，实现公共字段自动写入
 * @author JiaChaoYang
 * @date 2023/11/21 11:31
*/
public interface MetaObjectHandler {

    /**
     * 插入文档对象字段填充（用于插入时对公共字段的填充）
     *
     * @param document 插入文档对象
     */
    void insertFill(Document document);

    /**
     * 更新文档对象字段填充（用于更新时对公共字段的填充）
     *
     * @param document 插入文档对象
     */
    void updateFill(Document document);

}
