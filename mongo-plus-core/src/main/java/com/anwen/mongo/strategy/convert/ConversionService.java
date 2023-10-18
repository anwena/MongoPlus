package com.anwen.mongo.strategy.convert;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.strategy.convert.impl.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
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
 * 策略应用
 * @author JiaChaoYang
 **/
public class ConversionService {

    static Logger logger = LoggerFactory.getLogger(ConversionStrategy.class);

    private static final Map<Class<?>, ConversionStrategy> conversionStrategies = new HashMap<>();

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
    }

    /**
     * 添加自定义转换器(后期要改，从spring去做)
     * @param clazz map的key，使用转换器要转换的类,get的时候，会根据字段的Type拿转换器
     * @param conversionStrategy 转换器接口
     * @return void
     * @author JiaChaoYang
     * @date 2023/10/17 0:19
    */
    public static void appendConversion(Class<?> clazz,ConversionStrategy conversionStrategy){
        conversionStrategies.put(clazz,conversionStrategy);
        System.out.println("给map加值了："+ JSON.toJSONString(conversionStrategies));
    }

    public static void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Class<?> fieldType = field.getType();
        ConversionStrategy conversionStrategy = conversionStrategies.get(fieldType);
        if (conversionStrategy == null) {
            logger.debug("Unsupported field type: {}",fieldType.getSimpleName());
            conversionStrategy = conversionStrategies.get(Object.class);
        }
        conversionStrategy.convertValue(field, obj, fieldValue);
    }

}
