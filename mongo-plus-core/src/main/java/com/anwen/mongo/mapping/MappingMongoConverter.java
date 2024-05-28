package com.anwen.mongo.mapping;

import com.anwen.mongo.cache.global.ConversionCache;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.domain.MongoPlusWriteException;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.strategy.mapping.MappingStrategy;
import com.anwen.mongo.toolkit.BsonUtil;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;

/**
 * 将对象映射为Document
 *
 * @author JiaChaoYang
 **/
public class MappingMongoConverter extends AbstractMongoConverter {

    private Log log = LogFactory.getLog(MappingMongoConverter.class);

    private final SimpleTypeHolder simpleTypeHolder;

    public MappingMongoConverter(MongoPlusClient mongoPlusClient,SimpleTypeHolder simpleTypeHolder) {
        super(mongoPlusClient);
        this.simpleTypeHolder = simpleTypeHolder;
    }

    @Override
    public void write(Object sourceObj, Bson bson, TypeInformation typeInformation){
        typeInformation.getFields().stream()
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
        TypeInformation.of(sourceObj).getFields().stream()
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
        MappingStrategy<Object> mappingStrategy = getMappingStrategy(sourceObj.getClass());
        if (mappingStrategy != null){
            try {
                resultObj = mappingStrategy.mapping(sourceObj);
            } catch (IllegalAccessException e) {
                String error = String.format("Exception mapping %s to simple type", sourceObj.getClass().getName());
                log.error(error,e);
                throw new MongoPlusWriteException(error);
            }
        } else if (simpleTypeHolder.isSimpleType(sourceObj.getClass())) {
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
        //经过一下Document处理器
        if (HandlerCache.documentHandler != null && bson instanceof Document){
            HandlerCache.documentHandler.insertInvoke(Collections.singletonList((Document) bson));
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T read(Object sourceObj, TypeReference<T> typeReference) {
        Class<?> clazz = typeReference.getClazz();
        ConversionStrategy<?> conversionStrategy = getConversionStrategy(clazz);
        try {
            if (Collection.class.isAssignableFrom(clazz) && null == conversionStrategy){
                Type genericTypeClass = getGenericTypeClass((ParameterizedType) typeReference.getType(), 0);
                return (T) convertCollection(genericTypeClass,sourceObj,createCollectionInstance(clazz));
            }
            if (Map.class.isAssignableFrom(clazz) && null == conversionStrategy){
                Type genericTypeClass = getGenericTypeClass((ParameterizedType) typeReference.getType(), 1);
                return (T) convertMap(genericTypeClass,sourceObj,createMapInstance(clazz));
            } else if (null == conversionStrategy){
                conversionStrategy = ConversionCache.getConversionStrategy(Object.class);
            }
            return (T) conversionStrategy.convertValue(sourceObj, clazz , this);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 集合单独处理
     * @author anwen
     * @date 2024/5/6 下午1:14
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public Collection<?> convertCollection(Type type, Object fieldValue, Collection collection) {
        if (fieldValue == null){
            return collection;
        }
        // 如果fieldValue不是Collection类型，则将其转换为单元素的ArrayList
        if (!(fieldValue instanceof Collection<?>)) {
            Object finalFieldValue = fieldValue;
            fieldValue = new ArrayList<Object>() {{
                add(finalFieldValue);
            }};
        }
        //获取Type的Class
        Class<?> metaClass = getRawClass(type);
        // 处理集合元素
        List valueList = (ArrayList) fieldValue;
        if (simpleTypeHolder.isSimpleType(metaClass)) {
            // 如果泛型类型是简单类型，则直接添加到集合中
            valueList.forEach(value -> collection.add(convertValue(value, metaClass)));
        } else if (Collection.class.isAssignableFrom(metaClass)) {
            // 如果泛型类型是集合类型，则递归处理
            // 获取集合的泛型类型
            Type collectionType = getGenericTypeClass((ParameterizedType) type, 0);
            Collection<?> collectionInstance = createCollectionInstance(metaClass);
            convertCollection(collectionType, fieldValue, collectionInstance);
            collection.add(collectionInstance);
        } else if (Map.class.isAssignableFrom(metaClass)){
            valueList.forEach(value -> collection.add(convertMap(getGenericTypeClass((ParameterizedType) type, 1),value,createMapInstance(metaClass))));
        } else {
            valueList.forEach(value -> collection.add(readInternal((Document) value, metaClass)));
        }
        return collection;
    }

    @SuppressWarnings({"rawtypes","unchecked"})
    public <V> Map<String,V> convertMap(Type type, Object fieldValue, Map map){
        if(fieldValue == null){
            return map;
        }
        Document document = (Document) fieldValue;
        Class<?> rawClass = getRawClass(type);
        if (simpleTypeHolder.isSimpleType(rawClass)){
            document.forEach((k,v)-> map.put(k,convertValue(v,rawClass)));
        } else if (Collection.class.isAssignableFrom(rawClass)){
            document.forEach((k,v) -> map.put(k,convertCollection(getGenericTypeClass((ParameterizedType) type, 0),v,createCollectionInstance(rawClass))));
        } else if (Map.class.isAssignableFrom(rawClass)){
            document.forEach((k,v) -> map.put(k,convertMap(getGenericTypeClass((ParameterizedType) type, 1),v,createMapInstance(rawClass))));
        } else {
            document.forEach((k,v) -> map.put(k,readInternal((Document) v, rawClass)));
        }
        return map;
    }

    /**
     * 获取泛型的原始类
     */
    private Class<?> getRawClass(Type type) {
        if (type instanceof Class) {
            return (Class<?>) type;
        } else if (type instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) type).getRawType();
        } else {
            throw new RuntimeException("Unknown type: " + type);
        }
    }

    /**
     * 创建指定类型的集合实例
     */
    @SuppressWarnings("rawtypes")
    private Collection<?> createCollectionInstance(Class<?> collectionClass) {
        Collection collection;
        try {
            if (collectionClass.isInterface()){
                collection = new ArrayList();
            }else {
                collection = (Collection) collectionClass.getDeclaredConstructor().newInstance();
            }
        } catch (InstantiationException | InvocationTargetException | NoSuchMethodException | IllegalAccessException e) {
            throw new MongoPlusConvertException("Failed to create Collection instance",e);
        }
        return collection;
    }

    @SuppressWarnings("rawtypes")
    public Map createMapInstance(Class<?> mapClass){
        Map map;
        try {
            if (mapClass.isInterface()){
                map = new HashMap();
            } else {
                map = (Map) mapClass.getDeclaredConstructor().newInstance();
            }
        } catch (InstantiationException | InvocationTargetException | NoSuchMethodException | IllegalAccessException e) {
            throw new MongoPlusConvertException("Failed to create Map instance",e);
        }
        return map;
    }

    /**
     * 获取type的泛型
     * @author anwen
     * @date 2024/5/6 下午9:19
     */
    public static Type getGenericTypeClass(ParameterizedType parameterizedType,int size){
        return parameterizedType.getActualTypeArguments()[size];
    }

}
