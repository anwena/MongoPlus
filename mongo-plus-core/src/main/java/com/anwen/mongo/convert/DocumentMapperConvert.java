package com.anwen.mongo.convert;

import com.mongodb.client.ListIndexesIterable;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description: Document转对象
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 19:30
 * @Version: 1.0
 */
public class DocumentMapperConvert {

    public static List<Document> indexesIterableToDocument(ListIndexesIterable<Document> indexesIterable){
        return new ArrayList<Document>(){{
            try (MongoCursor<Document> cursor = indexesIterable.iterator()) {
                while (cursor.hasNext()) {
                    add(cursor.next());
                }
            }
        }};
    }
}
