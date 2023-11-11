package com.anwen.mongo.cache.codec;

import com.anwen.mongo.toolkit.codec.RegisterCodecUtil;
import com.mongodb.MongoClientSettings;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.ArrayList;
import java.util.List;

/**
 * 解码器缓存，执行器获取collection时，会来这里拿一下，需要时可以再这里给解码器赋值
 * 可以自实现{@link CodecRegistry}，也可以使用{@link RegisterCodecUtil}工具类中的registerCodec()方法，
 * 此方法会获取传入的类中的所有非java原生的类字段，逐一注册为解码器，返回List<CodecRegistry>
 * @author JiaChaoYang
 **/
public abstract class CodecRegistryCache {

    public static List<CodecRegistry> codecRegistryList = new ArrayList<>();

    static {
        codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
    }

    public static void addCodecRegistry(CodecRegistry codecRegistry){
        codecRegistryList.add(codecRegistry);
    }

    public static void addCodecRegistry(List<CodecRegistry> _codecRegistryList){
        codecRegistryList.addAll(_codecRegistryList);
    }

    public static List<CodecRegistry> getCodecRegistry(){
        return codecRegistryList;
    }

}
