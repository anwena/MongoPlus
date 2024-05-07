package com.anwen.mongo.strategy.convert;

import com.anwen.mongo.strategy.convert.impl.*;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;

/**
 * 策略应用
 * @author JiaChaoYang
 **/
public class ConversionService {

    private static final Map<Class<?>, ConversionStrategy<?>> conversionStrategies = new HashMap<>();

    static {
        conversionStrategies.put(Integer.class, new IntegerConversionStrategy());
        conversionStrategies.put(Long.class, new LongConversionStrategy());
        conversionStrategies.put(Double.class, new DoubleConversionStrategy());
        conversionStrategies.put(Float.class, new FloatConversionStrategy());
        conversionStrategies.put(Boolean.class, new BooleanConversionStrategy());
        conversionStrategies.put(String.class, new StringConversionStrategy());
        conversionStrategies.put(LocalTime.class,new LocalTimeConversionStrategy());
        conversionStrategies.put(LocalDate.class,new LocalDateConversionStrategy());
        conversionStrategies.put(LocalDateTime.class,new LocalDateTimeConversionStrategy());
        conversionStrategies.put(Date.class,new DateConversionStrategy());
        conversionStrategies.put(Instant.class,new InstantConversionStrategy());
        conversionStrategies.put(Object.class,new DefaultConversionStrategy());
        conversionStrategies.put(BigDecimal.class,new BigDecimalConversionStrategy());
        conversionStrategies.put(BigInteger.class,new BigIntegerConversionStrategy());
        conversionStrategies.put(Map.class,new MapConversionStrategy());
        conversionStrategies.put(Collection.class,new CollectionConversionStrategy());
        conversionStrategies.put(Enum.class,new EnumConversionStrategy<>());
    }

    /**
     * 添加自定义转换器，转换器需实现ConversionStrategy
     * 可以使用此方法添加，或者将转换器注册为Bean，MongoPlus会自动扫描到转换器，加入到转换器策略中
     * @param clazz map的key，使用转换器要转换的类,get的时候，会根据字段的Type拿转换器
     * @param conversionStrategy 转换器接口
     * @return void
     * @author JiaChaoYang
     * @date 2023/10/17 0:19
    */
    public static <T> void appendConversion(Class<?> clazz, ConversionStrategy<T> conversionStrategy){
        conversionStrategies.put(clazz,conversionStrategy);
    }

    /**
     * 获取所有的转换器
     * @author JiaChaoYang
     * @date 2023/11/3 9:50
    */
    public static Map<Class<?>, ConversionStrategy<?>> getAllConversion(){
        return conversionStrategies;
    }

    /**
     * 是否存在该类型的转换器
     * @param clazz 类型的class
     * @return boolean
     * @author JiaChaoYang
     * @date 2023/10/20 18:28
    */
    public static boolean isExist(Class<?> clazz){
        return conversionStrategies.containsKey(clazz);
    }

    public static ConversionStrategy<?> getConversion(Class<?> clazz){
        return conversionStrategies.get(clazz);
    }

    /**
     * 将字段进行转换，根据field的type
     * @author JiaChaoYang
     * @date 2023/11/3 10:43
    */
    public static Object convertValue(Field field, Object obj, Object fieldValue, Class<?>... clazz) throws IllegalAccessException {
        Class<?> fieldType = getFieldType(clazz, field);
        ConversionStrategy<?> conversionStrategy = getConversionStrategy(fieldType);

        return conversionStrategy.convertValue(field, obj, fieldValue);
    }

    private static Class<?> getFieldType(Class<?>[] clazz, Field field) {
        Class<?> fieldType = field.getType();
        if (clazz != null && clazz.length > 0) {
            fieldType = clazz[0];
        }
        if (Map.class.isAssignableFrom(fieldType)) {
            fieldType = Map.class;
        }
        if (Collection.class.isAssignableFrom(fieldType)){
            fieldType = Collection.class;
        }
        if (fieldType.isEnum()){
            fieldType = Enum.class;
        }
        return fieldType;
    }

    private static ConversionStrategy<?> getConversionStrategy(Class<?> fieldType) {
        ConversionStrategy<?> conversionStrategy = conversionStrategies.get(fieldType);
        if (conversionStrategy == null) {
            conversionStrategy = conversionStrategies.get(Object.class);
        }
        return conversionStrategy;
    }

    /**
     * 设置值
     * @author JiaChaoYang
     * @date 2023/11/3 10:42
    */
    public static void setValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Object value = convertValue(field, obj, fieldValue);
        field.set(obj,value);
    }

}
