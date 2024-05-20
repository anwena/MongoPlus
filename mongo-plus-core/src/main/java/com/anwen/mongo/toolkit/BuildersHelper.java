package com.anwen.mongo.toolkit;

import org.bson.BsonDocument;
import org.bson.BsonDocumentWriter;
import org.bson.codecs.Encoder;
import org.bson.codecs.EncoderContext;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.conversions.Bson;

/**
 * 拷贝com.mongodb.client.model.BuildersHelper，需要用到内部类
 * @author anwen
 * @date 2024/5/21 上午12:16
 */
public class BuildersHelper {

    @SuppressWarnings("unchecked")
    static <TItem> void encodeValue(final BsonDocumentWriter writer, final TItem value, final CodecRegistry codecRegistry) {
        if (value == null) {
            writer.writeNull();
        } else if (value instanceof Bson) {
            codecRegistry.get(BsonDocument.class).encode(writer,
                    ((Bson) value).toBsonDocument(BsonDocument.class, codecRegistry),
                    EncoderContext.builder().build());
        } else {
            ((Encoder) codecRegistry.get(value.getClass())).encode(writer, value, EncoderContext.builder().build());
        }
    }

    private BuildersHelper() {
    }

}
