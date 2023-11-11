package com.anwen.mongo.codec;

import org.bson.BsonReader;
import org.bson.BsonWriter;
import org.bson.codecs.Codec;
import org.bson.codecs.DecoderContext;
import org.bson.codecs.EncoderContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 集合加解码器
 * @date 2023-11-10 14:19
 **/

public class CollectionGenericCodec<T> implements Codec<T> {

    private final Logger logger = LoggerFactory.getLogger(GenericCodec.class);

    private final Class<T> clazz;

    private List<T> list;

    public CollectionGenericCodec(Class<T> clazz) {
        this.clazz = clazz;
    }

    @Override
    public T decode(BsonReader reader, DecoderContext decoderContext) {
        return null;
    }

    @Override
    public void encode(BsonWriter writer, T value, EncoderContext encoderContext) {
        writer.writeStartArray();
        writer.writeStartDocument();

    }

    @Override
    public Class<T> getEncoderClass() {
        return null;
    }
}
