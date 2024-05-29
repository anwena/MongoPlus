package com.anwen.mongo.cache.global;

import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.strategy.conversion.impl.*;
import org.bson.Document;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * 转换器
 * @author anwen
 * @date 2024/5/8 下午9:12
 */
public class ConversionCache {

    private static final Map<Class<?>, ConversionStrategy<?>> conversionStrategieMap = new HashMap<>();

    static {
        conversionStrategieMap.put(Integer.class,new IntegerConversionStrategy());
        conversionStrategieMap.put(Long.class, new LongConversionStrategy());
        conversionStrategieMap.put(Double.class, new DoubleConversionStrategy());
        conversionStrategieMap.put(Float.class, new FloatConversionStrategy());
        conversionStrategieMap.put(Boolean.class, new BooleanConversionStrategy());
        conversionStrategieMap.put(String.class, new StringConversionStrategy());
        conversionStrategieMap.put(LocalTime.class,new LocalTimeConversionStrategy());
        conversionStrategieMap.put(LocalDate.class,new LocalDateConversionStrategy());
        conversionStrategieMap.put(LocalDateTime.class,new LocalDateTimeConversionStrategy());
        conversionStrategieMap.put(Date.class,new DateConversionStrategy());
        conversionStrategieMap.put(Instant.class,new InstantConversionStrategy());
        conversionStrategieMap.put(Object.class,new DefaultConversionStrategy());
        conversionStrategieMap.put(BigDecimal.class,new BigDecimalConversionStrategy());
        conversionStrategieMap.put(BigInteger.class,new BigIntegerConversionStrategy());
        conversionStrategieMap.put(Enum.class,new EnumConversionStrategy<>());
        conversionStrategieMap.put(Document.class,new DocumentConversionStrategy());
        conversionStrategieMap.put(byte[].class,new ByteArrayConversionStrategy());
    }

    public static ConversionStrategy<?> getConversionStrategy(Class<?> clazz){
        return conversionStrategieMap.get(clazz);
    }

    public static void putConversionStrategy(Class<?> clazz,ConversionStrategy<?> conversionStrategy){
        conversionStrategieMap.put(clazz,conversionStrategy);
    }

}
