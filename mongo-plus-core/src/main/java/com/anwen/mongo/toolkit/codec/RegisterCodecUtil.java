package com.anwen.mongo.toolkit.codec;

import com.anwen.mongo.cache.CodecRegistryCache;
import com.anwen.mongo.codec.GenericCodec;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.DBObjectCodecProvider;
import com.mongodb.DBRefCodecProvider;
import com.mongodb.DocumentToDBRefTransformer;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.model.geojson.codecs.GeoJsonCodecProvider;
import org.bson.codecs.BsonValueCodecProvider;
import org.bson.codecs.DocumentCodecProvider;
import org.bson.codecs.ValueCodecProvider;
import org.bson.codecs.configuration.CodecProvider;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.codecs.pojo.PojoCodecProvider;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RegisterCodecUtil {

    static List<CodecRegistry> codecRegistryList = new ArrayList<>();

    private List<Class<?>> codecList = new ArrayList<Class<?>>(){{
        add(BigInteger.class);
        add(LocalDateTime.class);
        add(LocalDate.class);
        add(LocalTime.class);
        add(BigDecimal.class);
        add(Map.class);
        add(ConcurrentMap.class);
    }};

    static {
        codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
    }

    public static <T> List<CodecRegistry> registerCodec(T t){
        return registerCodec(ClassTypeUtil.getAllCustomFieldClasses(t.getClass()));
    }

    public static List<CodecRegistry> registerCodec(List<Class<?>> fieldClasses){
        if (CollUtil.isEmpty(CodecRegistryCache.getCodecRegistry())){
            codecRegistryList.addAll(CodecRegistryCache.getCodecRegistry());
        }
        fieldClasses.parallelStream().forEach(clazz -> codecRegistryList.add(CodecRegistries.fromCodecs(new GenericCodec<>(clazz))));
        return codecRegistryList;
    }

}
