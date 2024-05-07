package com.anwen.mongo.mapping;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.domain.MongoPlusWriteException;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.strategy.conversion.impl.IntegerConversionStrategy;
import com.anwen.mongo.toolkit.BsonUtil;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
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

    private static final Logger log = LoggerFactory.getLogger(MappingMongoConverter.class);
    private final SimpleTypeHolder simpleTypeHolder = new SimpleTypeHolder();

    private final Map<Class<?>, ConversionStrategy<?>> conversionStrategies = new HashMap<Class<?>, ConversionStrategy<?>>(){{
        put(Integer.class,new IntegerConversionStrategy());
    }};

    public MappingMongoConverter(MongoPlusClient mongoPlusClient) {
        super(mongoPlusClient);
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
        //经过一下Document处理器
        if (HandlerCache.documentHandler != null && bson instanceof Document){
            HandlerCache.documentHandler.insertInvoke(Collections.singletonList((Document) bson));
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T read(FieldInformation fieldInformation, Object sourceObj, Class<T> clazz) {
        ConversionStrategy<?> conversionStrategy = getConversionStrategy(clazz);
        try {
            if (Collection.class.isAssignableFrom(clazz) && null == conversionStrategy){
                Type type = getGenericTypeClass((ParameterizedType) fieldInformation.getField().getGenericType(), 0);
//                return (T) convertCollection(type,fieldInformation.getTypeClass(),sourceObj);
            }
            return (T) conversionStrategy.convertValue(sourceObj, fieldInformation.getTypeClass() , this);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    public <T> T convertValue(Object obj,Class<?> clazz){
        ConversionStrategy<?> conversionStrategy = getConversionStrategy(clazz);
        try {
            return (T) conversionStrategy.convertValue(obj,clazz,this);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    public static String getActualType(Object o,int index) {
        Type clazz = o.getClass().getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType)clazz;
        return pt.getActualTypeArguments()[index].toString();
    }

    /**
     * 集合单独处理
     * @author anwen
     * @date 2024/5/6 下午1:14
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public Collection<?> convertCollection(Type type, Object fieldValue, Collection collection) {
        // 如果fieldValue不是Collection类型，则将其转换为单元素的ArrayList
        if (!(fieldValue instanceof Collection<?>)) {
            Object finalFieldValue = fieldValue;
            fieldValue = new ArrayList<Object>() {{
                add(finalFieldValue);
            }};
        }

        // 获取集合的泛型类型
        Type collectionType = getGenericTypeClass((ParameterizedType) type, 0);
        Class<?> collectionClass = getRawClass(collectionType);

        // 处理集合元素
        List valueList = (ArrayList) fieldValue;
        if (simpleTypeHolder.isSimpleType(collectionClass)) {
            // 如果泛型类型是简单类型，则直接添加到集合中
            valueList.forEach(value -> {
                collection.add(convertValue(value, value.getClass()));
                System.out.printf("class：%s，值：%s%n", collectionClass, valueList);
            });
        } else if (Collection.class.isAssignableFrom(collectionClass)) {
            // 如果泛型类型是集合类型，则递归处理
            try {
                Collection<?> collectionInstance = createCollectionInstance(collectionClass);
                collection.addAll(convertCollection(collectionType, fieldValue, collectionInstance));
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        }
        return collection;
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
    private Collection<?> createCollectionInstance(Class<?> collectionClass) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {
        return collectionClass.isAssignableFrom(ArrayList.class) ? new ArrayList<>() : (Collection<?>) collectionClass.getDeclaredConstructor().newInstance();
    }


    @SuppressWarnings("rawtypes")
    public <V> Map<String,V> convertMap(ParameterizedType parameterizedType,Class<?> fieldType,Object fieldValue){
        Document document = (Document) fieldValue;
        Map map;
        try {
            map = fieldType.equals(Map.class) ? new HashMap() : (Map) fieldType.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | InvocationTargetException | NoSuchMethodException | IllegalAccessException e) {
            throw new MongoPlusConvertException("Failed to create a Map instance",e);
        }

        document.forEach((k,v)->{

        });
        return null;
    }

    public static void main(String[] args) {
        /*TypeInformation typeInformation = TypeInformation.of(Test.class);
        FieldInformation fieldInformation = typeInformation.getAnnotationField(ID.class,"");
        Type type = getGenericTypeClass((ParameterizedType) fieldInformation.getField().getGenericType(), 0);
        System.out.println(((ParameterizedType)type).getRawType());
        System.out.println(getGenericTypeClass(((ParameterizedType)type),0));*/
        Document document = new Document();
        document.put("list",new ArrayList<Integer>(){{
            add(1);
            add(2);
            add(3);
        }});
        MappingMongoConverter mappingMongoConverter = new MappingMongoConverter(null);
        TypeInformation typeInformation = TypeInformation.of(Test.class);
        FieldInformation annotationField = typeInformation.getAnnotationField(ID.class, "");
        List<Test> list = new ArrayList<>();
        Collection<?> objects = mappingMongoConverter.convertCollection(annotationField.getField().getGenericType(), document.get("list"), list);
        annotationField.setValue(objects);
        Test instance = typeInformation.getInstance();
        System.out.println(JSON.toJSONString(instance));
    }

    /**
     * 获取type的泛型
     * @author anwen
     * @date 2024/5/6 下午9:19
     */
    public static Type getGenericTypeClass(ParameterizedType parameterizedType,int size){
        return parameterizedType.getActualTypeArguments()[size];
    }

    private ConversionStrategy<?> getConversionStrategy(Class<?> target){
        Class<?> clazz = target;
        if (Map.class.isAssignableFrom(target)){
            clazz = Map.class;
        } else if (Collection.class.isAssignableFrom(target)){
            clazz = Collection.class;
        }
        ConversionStrategy<?> conversionStrategy = conversionStrategies.get(clazz);
        if (conversionStrategy == null){
            conversionStrategy = conversionStrategies.get(Object.class);
        }
        return conversionStrategy;
    }

}
