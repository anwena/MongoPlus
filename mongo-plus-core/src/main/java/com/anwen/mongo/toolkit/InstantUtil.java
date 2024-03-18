package com.anwen.mongo.toolkit;

import com.mongodb.MongoException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.*;

/**
 * 时间戳工具
 *
 * @author JiaChaoYang
 **/
public class InstantUtil {

    private static final Logger logger = LoggerFactory.getLogger(InstantUtil.class);

    /**
     * 时间戳转LocalDateTime
     * @param timestamp 时间戳
     * @author JiaChaoYang
     * @date 2023/10/18 22:46
    */
    public static LocalDateTime convertTimestampToLocalDateTime(long timestamp) {
        Instant instant;
        try {
            instant = Instant.ofEpochMilli(timestamp);
        }catch (Exception e){
            logger.error("Convert To Instant Fail,message: {}",e.getMessage(),e);
            throw new MongoException("Convert To Instant Fail");
        }
        return LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
    }

    public static LocalDateTime convertTimestampToLocalDateTime(Instant instant){
        return LocalDateTime.ofInstant(instant,ZoneId.systemDefault());
    }

    /**
     * 时间戳转LocalDate
     * 不使用LocalDate的ofInstant的方法，因为java8不可用，这样做可以兼容版本
     * @param timestamp 时间戳
     * @author JiaChaoYang
     * @date 2023/10/18 22:50
    */
    public static LocalDate convertTimestampToLocalDate(long timestamp){
        return convertTimestampToLocalDateTime(timestamp).toLocalDate();
    }

    public static LocalDate convertTimestampToLocalDate(Instant instant){
        return convertTimestampToLocalDateTime(instant).toLocalDate();
    }

    /**
     * 时间戳转LocalDate
     * 不使用LocalDate的ofInstant的方法，因为java8不可用，这样做可以兼容版本
     * @param timestamp 时间戳
     * @author JiaChaoYang
     * @date 2023/10/18 22:51
    */
    public static LocalTime convertTimestampToLocalTime(long timestamp){
        return convertTimestampToLocalDateTime(timestamp).toLocalTime();
    }

    public static LocalTime convertTimestampToLocalTime(Instant instant){
        return convertTimestampToLocalDateTime(instant).toLocalTime();
    }

}
