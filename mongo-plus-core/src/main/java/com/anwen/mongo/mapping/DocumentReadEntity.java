package com.anwen.mongo.mapping;

import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoIterable;
import org.bson.Document;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class DocumentReadEntity implements EntityRead {

    @Override
    public <T> T read(Document document, Class<T> target) {
        T instance;
        ClassInformation classInformation = SimpleClassInformation.of(target);
        try {
            instance = target.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new MongoPlusConvertException("Failed to create instance");
        }
        classInformation.getFields().forEach(fieldInformation -> {
            Object obj;
            String fieldName = fieldInformation.getName();
            if (fieldInformation.isId()){
                fieldName = SqlOperationConstant._ID;
            }
            obj = document.get(fieldName);
            if (obj != null){
                if (obj instanceof Document){
                    obj = read((Document) obj,fieldInformation.getType());
                }
                if (obj instanceof MongoCollection){
                    obj = colToList(obj,fieldInformation);
                }
                Method method = fieldInformation.setMethod();
                try {
                    method.invoke(instance,obj);
                } catch (IllegalAccessException | InvocationTargetException e) {
                    throw new MongoPlusFieldException("Failed to write value to " + fieldInformation.getField().getName() +" field",e);
                }
            }
        });
        return null;
    }

    public List<Object> colToList(Object bson, FieldInformation fieldInformation) {
        ParameterizedType pt = (ParameterizedType) fieldInformation.getField().getGenericType();// 获取列表的类型
        List<Object> objs = new ArrayList<>();
        @SuppressWarnings("unchecked")
        MongoCollection<Document> cols = (MongoCollection<Document>) bson;
        try (MongoCursor<Document> cursor = cols.find().iterator()) {
            while (cursor.hasNext()) {
                Document document = cursor.next();
                @SuppressWarnings("rawtypes")
                // 获取元素类型
                Class clz = (Class) pt.getActualTypeArguments()[0];
                @SuppressWarnings("unchecked")
                Object obj = read(document, clz);
                System.out.println(document);
                objs.add(obj);
            }
        }
        return objs;
    }

    private void readMapInternal(MongoIterable<Document> mongoIterable,Map<String, Object> targetMap){
        try (MongoCursor<Document> mongoCursor = mongoIterable.iterator()) {
            while (mongoCursor.hasNext()){
                Document document = mongoCursor.next();
            }
        }
    }

    private void readMapProperty(Document document,Map<String, Object> targetMap){

    }
}
