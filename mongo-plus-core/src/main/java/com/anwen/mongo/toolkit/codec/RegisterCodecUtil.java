package com.anwen.mongo.toolkit.codec;

import com.anwen.mongo.cache.CodecRegistryCache;
import com.anwen.mongo.codec.GenericCodec;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.MongoClientSettings;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class RegisterCodecUtil {

    static List<CodecRegistry> codecRegistryList = new ArrayList<>();

    public static <T> List<CodecRegistry> registerCodec(T t){
        return registerCodec(ClassTypeUtil.getAllCustomFieldClasses(t.getClass()));
    }

    public static List<CodecRegistry> registerCodec(List<Class<?>> fieldClasses){
        if (CollUtil.isEmpty(CodecRegistryCache.getCodecRegistry())){
            codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
        }
        fieldClasses.parallelStream().forEach(clazz -> codecRegistryList.add(CodecRegistries.fromCodecs(new GenericCodec<>(clazz))));
        return codecRegistryList;
    }

}
