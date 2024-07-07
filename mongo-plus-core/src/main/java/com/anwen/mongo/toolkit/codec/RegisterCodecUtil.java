package com.anwen.mongo.toolkit.codec;

import com.mongodb.MongoClientSettings;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class RegisterCodecUtil {

    private static final List<CodecRegistry> codecRegistryList = new CopyOnWriteArrayList<>();

    static {
        // 静态加载所有默认解码器
        codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
    }

    public static CodecRegistry getCodecCacheAndDefault() {
        return CodecRegistries.fromRegistries(codecRegistryList);
    }

}
