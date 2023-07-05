package com.anwen.mongo.utils.codec;

import com.anwen.mongo.codec.GenericCodec;
import com.anwen.mongo.utils.ClassTypeUtil;
import com.mongodb.MongoClientSettings;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.ArrayList;
import java.util.List;

public class RegisterCodecUtil {

    static List<CodecRegistry> codecRegistryList = new ArrayList<>();

    public static <T> List<CodecRegistry> registerCodec(T t){
        if (codecRegistryList.size() == 0){
            codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
        }
        List<Class<?>> fieldClasses = ClassTypeUtil.getAllCustomFieldClasses(t.getClass());
        fieldClasses.parallelStream().forEach(clazz -> {
            codecRegistryList.add(CodecRegistries.fromCodecs(new GenericCodec<>(clazz)));
        });
        return codecRegistryList;
    }

}
