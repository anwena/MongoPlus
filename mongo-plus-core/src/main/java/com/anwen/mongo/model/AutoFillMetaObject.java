package com.anwen.mongo.model;

import com.anwen.mongo.bson.MongoPlusDocument;
import com.anwen.mongo.support.SFunction;
import org.bson.Document;

/**
 * 自动填充元对象
 * @author JiaChaoYang
 * @date 2024/5/1 下午9:36
 */
public class AutoFillMetaObject {

    /**
     * 需要自动填充的字段
     * @date 2024/6/4 下午9:43
     */
    private final MongoPlusDocument document;

    /**
     * 自动填充最终的值
     * @date 2024/6/4 下午9:43
     */
    private final MongoPlusDocument autoFillDocument;

    public AutoFillMetaObject() {
        this.document = new MongoPlusDocument();
        this.autoFillDocument = new MongoPlusDocument();
    }

    public AutoFillMetaObject(MongoPlusDocument document) {
        this.document = document;
        this.autoFillDocument = new MongoPlusDocument();
    }

    public AutoFillMetaObject(MongoPlusDocument document,MongoPlusDocument autoFillDocument) {
        this.document = document;
        this.autoFillDocument = autoFillDocument;
    }

    /**
     * 获取所有的自动填充字段
     * @return {@link MongoPlusDocument}
     * @author anwen
     * @date 2024/5/1 下午10:14
     */
    public MongoPlusDocument getAllFillField() {
        return autoFillDocument;
    }

    public MongoPlusDocument getDocument() {
        return document;
    }

    public void getAllFillFieldAndClear(Document document){
        document.putAll(autoFillDocument);
        autoFillDocument.clear();
    }

    /**
     * 是否存在自动填充的字段
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/1 下午10:14
     */
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
            autoFillDocument.put(column, value);
        }
    }

    public <T,R> void forceFillValue(SFunction<T,R> column,Object value){

    }

    /**
     * 设置自动填充内容
     * @param column 列名
     * @param value 值
     * @author anwen
     * @date 2024/5/1 下午10:14
     */
    public void fillValue(String column,Object value){
        if (metaObjectExist(column)) {
            autoFillDocument.put(column, value);
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
     * 指定的自动填充字段是否存在
     * @param column 列名
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/1 下午10:15
     */
    public boolean metaObjectExist(String column){
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
        return autoFillDocument.get(column);
    }

    /**
     * 获取自动填充字段现有得值
     * @param column 列名
     * @return {@link Object}
     * @author anwen
     * @date 2024/5/1 下午10:15
     */
    public Object getMetaObjectValue(String column){
        return autoFillDocument.get(column);
    }

}
