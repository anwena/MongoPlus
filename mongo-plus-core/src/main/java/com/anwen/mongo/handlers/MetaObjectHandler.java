package com.anwen.mongo.handlers;

import com.anwen.mongo.model.AutoFillMetaObject;

/**
 * 元对象(Document当做‘元对象’)字段填充控制器抽象类，实现公共字段自动写入
 * @author JiaChaoYang
 * @date 2023/11/21 11:31
 * @since quote from: MyBatisPlus
*/
public interface MetaObjectHandler {

    /**
     * 插入文档对象字段填充（用于插入时对公共字段的填充）
     *
     * @param insertAutoFillMetaObject 插入文档字段，这里只有设置了自动填充的字段
     */
    void insertFill(AutoFillMetaObject insertAutoFillMetaObject);

    /**
     * 更新文档对象字段填充（用于更新时对公共字段的填充）
     *
     * @param updateAutoFillMetaObject 更新文档字段，这里只有设置了自动填充的字段
     */
    void updateFill(AutoFillMetaObject updateAutoFillMetaObject);

}
