package com.anwen.mongo.mapping;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.domain.MongoPlusWriteException;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.toolkit.BsonUtil;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.*;

/**
 * 将对象映射为Document
 *
 * @author JiaChaoYang
 **/
public class MappingMongoConverter extends AbstractMongoConverter {

    private final SimpleTypeHolder simpleTypeHolder = new SimpleTypeHolder();

    public MappingMongoConverter(MongoPlusClient mongoPlusClient) {
        super(mongoPlusClient);
    }

    @Override
    public void write(Object sourceObj, Bson bson, ClassInformation classInformation){
        classInformation.getFields().stream()
                .filter(fieldInformation -> !fieldInformation.isSkipCheckField() && !fieldInformation.isId())
                .forEach(fieldInformation -> writeProperties(bson, fieldInformation.getName(), fieldInformation.getValue()));
    }

    /**
     * 写入内部对象
     * @param sourceObj 源对象
     * @param bson bson
     * @return {@link Bson}
     * @author anwen
     * @date 2024/5/1 下午11:44
     */
    public Bson writeInternal(Object sourceObj, Bson bson){
        SimpleClassInformation.of(sourceObj).getFields().stream()
                .filter(fieldInformation -> !fieldInformation.isSkipCheckField())
                .forEach(fieldInformation -> writeProperties(bson, fieldInformation.getName(), fieldInformation.getValue()));
        return bson;
    }

    /**
     * 属性写入Bson中
     * @param bson bson
     * @param key key
     * @param sourceObj 源对象
     * @author anwen
     * @date 2024/5/1 下午11:45
     */
    private void writeProperties(Bson bson,String key,Object sourceObj){
        if (PropertyCache.ignoringNull && sourceObj == null){
            return;
        }
        BsonUtil.addToMap(bson, key, writeProperties(sourceObj));
    }

    /**
     * 属性映射
     * @param sourceObj 源对象
     * @return {@link Object}
     * @author anwen
     * @date 2024/5/1 下午11:46
     */
    private Object writeProperties(Object sourceObj){
        Object resultObj;
        if (sourceObj == null || simpleTypeHolder.isSimpleType(sourceObj.getClass())) {
            resultObj = getPotentiallyConvertedSimpleWrite(sourceObj);
        } else if (sourceObj instanceof Collection || sourceObj.getClass().isArray()) {
            resultObj = writeCollectionInternal(BsonUtil.asCollection(sourceObj), new ArrayList<>());
        } else if (Map.class.isAssignableFrom(sourceObj.getClass())) {
            resultObj = writeMapInternal((Map<?, ?>) sourceObj,new Document());
        } else {
            resultObj = writeInternal(sourceObj,new Document());
        }
        return resultObj;
    }

    /**
     * map类型的处理
     * @param obj 源对象
     * @param bson bson
     * @return {@link org.bson.conversions.Bson}
     * @author anwen
     * @date 2024/5/1 下午11:46
     */
    private Bson writeMapInternal(Map<?,?> obj,Bson bson){
        //循环map
        obj.forEach((k,v)->{
            //如果key是简单类型
            if (simpleTypeHolder.isSimpleType(k.getClass())){
                writeProperties(bson,String.valueOf(k),v);
            }else {
                throw new MongoPlusWriteException("Cannot use a complex object as a key value");
            }
        });
        return bson;
    }

    /**
     * 集合类型的处理
     * @param obj 源对象
     * @param sink 集合
     * @return {@link Collection<?>}
     * @author anwen
     * @date 2024/5/1 下午11:46
     */
    @SuppressWarnings(value = "unchecked")
    private Collection<?> writeCollectionInternal(Collection<?> obj, Collection<?> sink){
        List<Object> collection = sink instanceof List ? (List<Object>) sink : new ArrayList<>(sink);
        obj.forEach(element -> collection.add(writeProperties(element)));
        return collection;
    }

    @Override
    public void write(Map<?, ?> map, Bson bson) {
        writeMapInternal(map,bson);
    }

}
