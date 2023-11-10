package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description Date类型转换器策略实现
 * @date 2023-10-17 10:40
 **/
public class DateConversionStrategy implements ConversionStrategy<Date> {

    Logger logger = LoggerFactory.getLogger(DateConversionStrategy.class);

    //定义格式集合
    private final List<String> formatList = new ArrayList<String>(){{
        add("yyyy-MM-dd");
        add("MM/dd/yyyy");
        add("dd-MMM-yyyy");
        add("EEE, dd MMM yyyy HH:mm:ss zzz");
        add("yyyy-MM-dd HH:mm:ss");
        add("HH:mm:ss");
    }};

    @Override
    public Date convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Date date;
        if (fieldValue.getClass().equals(Long.class)){
            date = new Date((Long) fieldValue);
        }else {
            /*for (String format : formatList) {
                try {
                    SimpleDateFormat dateFormat = new SimpleDateFormat(format);
                    date = dateFormat.parse(String.valueOf(fieldValue));
                    break;
                } catch (ParseException ignored) {
                }
            }
            if (null == date) {
                logger.error("Unrecognized date format");
                throw new IllegalAccessException("Unrecognized date format");
            }*/
            date = (Date) fieldValue;
        }
        return date;
    }
}
