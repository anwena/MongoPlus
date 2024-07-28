package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.annotation.comm.FieldEncrypt;
import com.anwen.mongo.cache.global.ConversionCache;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.cache.global.SimpleCache;
import com.anwen.mongo.domain.MongoPlusWriteException;
import com.anwen.mongo.handlers.TypeHandler;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.strategy.mapping.MappingStrategy;
import com.anwen.mongo.toolkit.BsonUtil;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.EncryptorUtil;
import com.anwen.mongo.toolkit.StringUtils;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.Binary;
import org.bson.types.ObjectId;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 将对象映射为Document
 *
 * @author JiaChaoYang
 **/
public class MappingMongoConverter extends AbstractMongoConverter {

    private final Log log = LogFactory.getLog(MappingMongoConverter.class);

    private final SimpleTypeHolder simpleTypeHolder;

    private List<Class<?>> ignoreType = new ArrayList<>();

    private final Map<Type, Class<?>> typeClassCache = new ConcurrentHashMap<>();

    private final Map<Type, Type> genericTypeCache = new ConcurrentHashMap<>();

    public MappingMongoConverter(MongoPlusClient mongoPlusClient) {
        super(mongoPlusClient);
        this.simpleTypeHolder = SimpleCache.getSimpleTypeHolder();
        ignoreType.add(ObjectId.class);
        ignoreType.add(Binary.class);
    }

    public MappingMongoConverter(MongoPlusClient mongoPlusClient,List<Class<?>> ignoreType){
        super(mongoPlusClient);
        this.simpleTypeHolder = SimpleCache.getSimpleTypeHolder();
        this.ignoreType = ignoreType;
    }

    @Override
    public void write(Object sourceObj, Bson bson, TypeInformation typeInformation) {
        processFields(typeInformation.getFields(), bson, true);
    }

    /**
     * 写入内部对象
     * @param sourceObj 源对象
     * @param bson bson
     * @return {@link Bson}
     */
    public Bson writeInternal(Object sourceObj, Bson bson) {
        processFields(TypeInformation.of(sourceObj).getFields(), bson, false);
        return bson;
    }

    /**
     * 处理字段信息并写入 BSON
     * @param fields 字段信息列表
     * @param bson BSON 对象
     * @param filterId 是否过滤掉 ID 字段
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    private void processFields(List<FieldInformation> fields, Bson bson, boolean filterId) {
        fields.stream()
                .filter(fieldInformation -> !fieldInformation.isSkipCheckField() && (!filterId || !fieldInformation.isId()))
                .forEach(fieldInformation -> {
                    CollectionField collectionField = fieldInformation.getCollectionField();
                    Object obj = null;
                    if (collectionField != null && ClassTypeUtil.isTargetClass(TypeHandler.class,collectionField.typeHandler())) {
                        TypeHandler typeHandler = (TypeHandler)ClassTypeUtil.getInstanceByClass(collectionField.typeHandler());
                        obj = typeHandler.setParameter(fieldInformation.getName(),fieldInformation.getValue());
                    }
                    String fieldName = fieldInformation.getName();
                    if (collectionField == null && PropertyCache.camelToUnderline){
                        fieldName = StringUtils.camelToUnderline(fieldName);
                    }
                    if (fieldInformation.isAnnotation(FieldEncrypt.class)){
                        obj = EncryptorUtil.encrypt((FieldEncrypt) fieldInformation.getAnnotation(FieldEncrypt.class),fieldInformation.getValue());
                    }
                    if (ignoreType.contains(fieldInformation.getTypeClass())){
                        obj = fieldInformation.getValue();
                    }
                    //如果类型处理器返回null，则继续走默认处理
                    if (obj != null) {
                        BsonUtil.addToMap(bson, fieldName, obj);
                    } else {
                        writeProperties(bson, fieldName, fieldInformation.getValue());
                    }
                });
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
        MappingStrategy<Object> mappingStrategy = null;
        if (sourceObj != null) {
            mappingStrategy = getMappingStrategy(sourceObj.getClass());
        }
        if (mappingStrategy != null){
            try {
                resultObj = mappingStrategy.mapping(sourceObj);
            } catch (IllegalAccessException e) {
                String error = String.format("Exception mapping %s to simple type", sourceObj.getClass().getName());
                log.error(error,e);
                throw new MongoPlusWriteException(error);
            }
        } else if (sourceObj == null || simpleTypeHolder.isSimpleType(sourceObj.getClass())) {
            resultObj = getPotentiallyConvertedSimpleWrite(sourceObj);
        } else if (ClassTypeUtil.isTargetClass(Collection.class,sourceObj.getClass()) || sourceObj.getClass().isArray()) {
            resultObj = writeCollectionInternal(BsonUtil.asCollection(sourceObj), new ArrayList<>());
        } else if (ClassTypeUtil.isTargetClass(Map.class,sourceObj.getClass())) {
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
     * @return {@link Bson}
     * @author anwen
     * @date 2024/5/1 下午11:46
     */
    @Override
    public Bson writeMapInternal(Map<?,?> obj,Bson bson) {
        //循环map
        obj.forEach((k,v)->{
            //如果key是简单类型
            if (simpleTypeHolder.isSimpleType(k.getClass())){
                String key = String.valueOf(k);
                if (PropertyCache.camelToUnderline){
                    key = StringUtils.camelToUnderline(key);
                }
                writeProperties(bson,key,v);
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
    public <T> T readInternal(Object sourceObj, TypeReference<T> typeReference){
        Class<?> clazz = typeReference.getClazz();
        ConversionStrategy<?> conversionStrategy = getConversionStrategy(clazz);

        try {
            if (ClassTypeUtil.isTargetClass(Collection.class,clazz)) {
                return handleCollectionType(sourceObj, typeReference, clazz, conversionStrategy);
            } else if (ClassTypeUtil.isTargetClass(Map.class,clazz)) {
                return handleMapType(sourceObj, typeReference, clazz, conversionStrategy);
            } else {
                return handleDefaultType(sourceObj, clazz, conversionStrategy);
            }
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private <T> T handleCollectionType(Object sourceObj, TypeReference<T> typeReference, Class<?> clazz, ConversionStrategy<?> conversionStrategy) throws IllegalAccessException {
        if (conversionStrategy == null) {
            Type genericTypeClass = extractGenericType(typeReference, 0);
            return (T) convertCollection(genericTypeClass, sourceObj, createCollectionInstance(clazz));
        }
        return (T) conversionStrategy.convertValue(sourceObj, clazz, this);
    }

    @SuppressWarnings("unchecked")
    private <T> T handleMapType(Object sourceObj, TypeReference<T> typeReference, Class<?> clazz, ConversionStrategy<?> conversionStrategy) throws IllegalAccessException {
        if (conversionStrategy == null) {
            Type genericTypeClass = extractGenericType(typeReference, 1);
            return (T) convertMap(genericTypeClass, sourceObj, createMapInstance(clazz));
        }
        return (T) conversionStrategy.convertValue(sourceObj, clazz, this);
    }

    @SuppressWarnings("unchecked")
    private <T> T handleDefaultType(Object sourceObj, Class<?> clazz, ConversionStrategy<?> conversionStrategy) throws IllegalAccessException {
        if (conversionStrategy == null) {
            conversionStrategy = ConversionCache.getConversionStrategy(Object.class);
        }
        return (T) conversionStrategy.convertValue(sourceObj, clazz, this);
    }

    private Type extractGenericType(TypeReference<?> typeReference, int index) {
        return genericTypeCache.computeIfAbsent(typeReference.getType(), type -> {
            if (type instanceof ParameterizedType) {
                return getGenericTypeClass((ParameterizedType) type, index);
            }
            return Object.class;
        });
    }

    /**
     * 获取type的泛型
     * @author anwen
     * @date 2024/5/6 下午9:19
     */
    public static Type getGenericTypeClass(ParameterizedType parameterizedType,int size){
        return parameterizedType.getActualTypeArguments()[size];
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
        } else if (ClassTypeUtil.isTargetClass(Collection.class,metaClass)) {
            // 如果泛型类型是集合类型，则递归处理
            // 获取集合的泛型类型
            Type collectionType = getGenericTypeClass((ParameterizedType) type, 0);
            Collection<?> collectionInstance = createCollectionInstance(metaClass);
            valueList.forEach(value -> convertCollection(collectionType, value, collectionInstance));
            collection.add(collectionInstance);
        } else if (ClassTypeUtil.isTargetClass(Map.class,metaClass)){
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
        } else if (ClassTypeUtil.isTargetClass(Collection.class,rawClass)){
            document.forEach((k,v) -> map.put(k,convertCollection(getGenericTypeClass((ParameterizedType) type, 0),v,createCollectionInstance(rawClass))));
        } else if (ClassTypeUtil.isTargetClass(Map.class,rawClass)){
            document.forEach((k,v) -> map.put(k,convertMap(getGenericTypeClass((ParameterizedType) type, 1),v,createMapInstance(rawClass))));
        } else {
            document.forEach((k,v) -> map.put(k,readInternal((Document) v, rawClass)));
        }
        return map;
    }

    private Class<?> getRawClass(Type type) {
        return typeClassCache.computeIfAbsent(type, this::computeRawClass);
    }

    private Class<?> computeRawClass(Type type) {
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
        if (collectionClass.isInterface()){
            collection = new ArrayList();
        }else {
            collection = (Collection) ClassTypeUtil.getInstanceByClass(collectionClass);
        }
        return collection;
    }

    @SuppressWarnings("rawtypes")
    public Map createMapInstance(Class<?> mapClass){
        Map map;
        if (mapClass.isInterface()){
            map = new HashMap();
        } else {
            map = (Map) ClassTypeUtil.getInstanceByClass(mapClass);
        }
        return map;
    }

}
