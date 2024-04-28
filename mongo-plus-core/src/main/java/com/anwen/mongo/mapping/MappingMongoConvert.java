package com.anwen.mongo.mapping;

import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.domain.MongoPlusWriteException;
import com.anwen.mongo.toolkit.BeanMapUtilByReflect;
import com.anwen.mongo.toolkit.BsonUtil;
import com.anwen.mongo.toolkit.CollUtil;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.*;

/**
 * 将对象映射为Document
 *
 * @author JiaChaoYang
 **/
public class MappingMongoConvert implements MongoConvert {

    //定义添加自动填充字段
    private final Map<String,Object> insertFillMap = new LinkedHashMap<>();
    private final Map<String,Object> updateFillMap = new LinkedHashMap<>();

    private final SimpleTypeHolder simpleTypeHolder = new SimpleTypeHolder();

    @Override
    public void write(Object sourceObj, Bson bson) {
        if (null == sourceObj) {
            return;
        }
        writeInternal(sourceObj,bson != null ? bson : new Document());
    }

    // TODO ID策略需要外挂
    @SuppressWarnings(value = "unchecked")
    private Bson writeInternal(Object sourceObj,Bson bson){
        ClassInformation classInformation = SimpleClassInformation.of(sourceObj);
        classInformation.getFields().forEach(fieldInformation -> {
            if (fieldInformation.isSkipCheckField()){
                return;
            }
            if (CollUtil.isEmpty(insertFillMap) || CollUtil.isEmpty(updateFillMap)){
                BeanMapUtilByReflect.getFillInsertAndUpdateField(classInformation,insertFillMap,updateFillMap);
            }
            Object obj;
            //如果是map
            if (fieldInformation.isMap()){
                obj = writeMapInternal((Map<Object, Object>) fieldInformation.getValue(),new Document());
            }
            //如果是集合
            else if (fieldInformation.isCollection()){
                Collection<?> collection = (Collection<?>) fieldInformation.getValue();
                obj = writeCollectionInternal(collection,new ArrayList<>(collection.size()));
            } else if (fieldInformation.isSimpleType()){
                obj = getPotentiallyConvertedSimpleWrite(fieldInformation.getValue());
            } else {
                obj = writeInternal(fieldInformation.getValue(),new Document());
            }
            BsonUtil.addToMap(bson,fieldInformation.getName(),obj);
            if (HandlerCache.metaObjectHandler != null){
                HandlerCache.metaObjectHandler.insertFill(insertFillMap,(Document) bson);
                HandlerCache.metaObjectHandler.updateFill(updateFillMap,(Document) bson);
            }
        });
        return bson;
    }

    private Bson writeMapInternal(Map<Object,Object> obj,Bson bson){
        //循环map
        obj.forEach((k,v)->{
            //如果key是简单类型
            if (simpleTypeHolder.isSimpleType(k.getClass())){
                //转为String，因为Mongo只接受Key为String
                String simpleKey = String.valueOf(k);
                //如果value为null，或者value是简单类型（这里可以通过配置决定为null是否添加）
                if (v == null || simpleTypeHolder.isSimpleType(v.getClass())) {
                    //将kv添加到Bson中
                    BsonUtil.addToMap(bson, simpleKey, getPotentiallyConvertedSimpleWrite(v));
                    //如果value是集合或数组
                } else if (v instanceof Collection || v.getClass().isArray()) {
                    //将kv添加到Bson中，value经过一下collection处理
                    BsonUtil.addToMap(bson, simpleKey,
                            writeCollectionInternal(BsonUtil.asCollection(v), new ArrayList<>()));
                } else {
                    //定义结果bson
                    Document document = new Document();
                    //递归重新走写入
                    writeInternal(v, document);
                    //将Document写入bson中
                    BsonUtil.addToMap(bson, simpleKey, document);
                }
            }else {
                throw new MongoPlusWriteException("Cannot use a complex object as a key value");
            }
        });
        return bson;
    }

    @SuppressWarnings(value = "unchecked")
    private Collection<?> writeCollectionInternal(Collection<?> obj, Collection<?> sink){

        List<Object> collection = sink instanceof List ? (List<Object>) sink : new ArrayList<>(sink);

        obj.forEach(element -> {
            Class<?> elementType = element == null ? null : element.getClass();
            if (elementType == null || simpleTypeHolder.isSimpleType(elementType)) {
                collection.add(getPotentiallyConvertedSimpleWrite(element));
            } else if (element instanceof Collection || elementType.isArray()) {

                Collection<?> objects = BsonUtil.asCollection(element);
                collection.add(writeCollectionInternal(objects, new ArrayList<>(objects.size())));
            } else {
                Document document = new Document();
                writeInternal(element, document);
                collection.add(document);
            }
        });
        return collection;
    }

    @Override
    public void write(Map<String, Object> map, Bson bson) {

    }

    @Override
    public void read(Document document, Class<?> clazz, List<?> resultList) {

    }

    @Override
    public Map<String, Object> read(Document document) {
        return Collections.emptyMap();
    }

    private Object getPotentiallyConvertedSimpleWrite(Object value) {

        if (value == null) {
            return null;
        }

        if (CollUtil.isArray(value)) {

            if (value instanceof byte[]) {
                return value;
            }
            return BsonUtil.asCollection(value);
        }

        return Enum.class.isAssignableFrom(value.getClass()) ? ((Enum<?>) value).name() : value;
    }

}
