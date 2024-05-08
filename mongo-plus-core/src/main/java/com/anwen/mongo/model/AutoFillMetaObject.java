package com.anwen.mongo.model;

import com.anwen.mongo.bson.MongoPlusDocument;
import com.anwen.mongo.support.SFunction;

/**
 * 自动填充元对象
 * @author JiaChaoYang
 * @date 2024/5/1 下午9:36
 */
public class AutoFillMetaObject {

    private final MongoPlusDocument document;

    public AutoFillMetaObject(MongoPlusDocument document) {
        this.document = document;
    }

    /**
     * 获取所有的自动填充字段
     * @return {@link MongoPlusDocument}
     * @author anwen
     * @date 2024/5/1 下午10:14
     */
    public MongoPlusDocument getAllFillField() {
        return document;
    }

    public boolean isEmpty() {
        return document.isEmpty();
    }

    /**
     * 设置自动填充内容
     * @param column 列名
     * @param value 值
     * @author anwen
     * @date 2024/5/1 下午10:14
     */
    public <T,R> void fillValue(SFunction<T,R> column,Object value){
        if (metaObjectExist(column)) {
            document.put(column, value);
        }
    }

    /**
     * 指定的自动填充字段是否存在
     * @param column 列名
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/1 下午10:15
     */
    public <T,R> boolean metaObjectExist(SFunction<T,R> column){
        return document.containsKey(column);
    }

    /**
     * 获取自动填充字段现有得值
     * @param column 列名
     * @return {@link Object}
     * @author anwen
     * @date 2024/5/1 下午10:15
     */
    public <T,R> Object getMetaObjectValue(SFunction<T,R> column){
        return document.get(column);
    }

}
