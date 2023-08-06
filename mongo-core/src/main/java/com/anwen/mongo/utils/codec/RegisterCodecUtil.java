package com.anwen.mongo.utils.codec;

import com.anwen.mongo.codec.GenericCodec;
import com.anwen.mongo.utils.ClassTypeUtil;
import com.mongodb.MongoClientSettings;
import org.bson.BsonReader;
import org.bson.BsonWriter;
import org.bson.Document;
import org.bson.codecs.Codec;
import org.bson.codecs.DecoderContext;
import org.bson.codecs.EncoderContext;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class RegisterCodecUtil {

    static List<CodecRegistry> codecRegistryList = new ArrayList<>();

    public static <T> List<CodecRegistry> registerCodec(T t){
        if (codecRegistryList.isEmpty()){
            codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
        }
        List<Class<?>> fieldClasses = ClassTypeUtil.getAllCustomFieldClasses(t.getClass());
        fieldClasses.parallelStream().forEach(clazz -> {
            codecRegistryList.add(CodecRegistries.fromCodecs(new GenericCodec<>(clazz)));
        });
        return codecRegistryList;
    }

/*    public static <T> List<CodecRegistry> registerCodecMap(Map<String,Object> map){
        // 创建自定义编码器和解码器
        Codec<Map<String, Object>> mapCodec = new Codec<Map<String, Object>>() {
            @Override
            public Map<String, Object> decode(org.bson.BsonReader reader, DecoderContext decoderContext) {
                throw new UnsupportedOperationException("Decoding is not supported.");
            }

            @Override
            public void encode(BsonWriter writer, Map<String, Object> value, EncoderContext encoderContext) {
                Document document = new Document(value);
                encoderContext.encodeWithChildContext(Document.class, writer, document);
            }

            @Override
            public Class<Map<String, Object>> getEncoderClass() {
                return (Class<Map<String, Object>>) (Class<?>) Map.class;
            }
        };

    }*/

}
