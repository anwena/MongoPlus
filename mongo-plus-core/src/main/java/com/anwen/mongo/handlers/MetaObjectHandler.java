package com.anwen.mongo.handlers;

import com.anwen.mongo.model.AutoFillMetaObject;
import org.bson.Document;

import java.util.Map;

/**
 * 元对象(Document当做‘元对象’)字段填充控制器抽象类，实现公共字段自动写入
 * @author JiaChaoYang
 * @date 2023/11/21 11:31
 * @since quote from: MyBatisPlus
*/
public interface MetaObjectHandler {

    /**
     * 插入文档对象字段填充（用于插入时对公共字段的填充）
     * 已弃用，请使用{@link #insertFill(AutoFillMetaObject)}
     * @param insertFillMap 插入文档字段，这里只有设置了自动填充的字段
     * @param document 插入文档对象
     */
    @Deprecated
    void insertFill(Map<String,Object> insertFillMap,Document document);

    /**
     * 更新文档对象字段填充（用于更新时对公共字段的填充）
     * 已弃用，请使用{@link #updateFill(AutoFillMetaObject)}
     * @param updateFillMap 更新文档字段，这里只有设置了自动填充的字段
     * @param document 更新文档对象
     */
    @Deprecated
    void updateFill(Map<String,Object> updateFillMap,Document document);


    /**
     * 插入文档对象字段填充（用于插入时对公共字段的填充）
     *
     * @param insertAutoFillMetaObject 插入文档字段，这里只有设置了自动填充的字段
     */
    default void insertFill(AutoFillMetaObject insertAutoFillMetaObject){}

    /**
     * 更新文档对象字段填充（用于更新时对公共字段的填充）
     *
     * @param insertAutoFillMetaObject 更新文档字段，这里只有设置了自动填充的字段
     */
    default void updateFill(AutoFillMetaObject insertAutoFillMetaObject){}

}
