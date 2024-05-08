package com.anwen.mongo.mapping;

import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoIterable;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * 将对象映射为Document和将Document映射为对象
 * @author JiaChaoYang
 * @date 2024/4/16 下午9:10
*/ 
public interface MongoConverter extends MongoWriter,EntityRead {

    /**
     * 添加的映射器
     * @author JiaChaoYang
     * @date 2024/5/1 下午11:52
     */
    void writeBySave(Object sourceObj, Document document);

    default Document writeBySave(Object sourceObj){
        Document document = new Document();
        writeBySave(sourceObj,document);
        return document;
    }

    default Document write(Map<String,Object> map){
        Document document = new Document();
        write(map,document);
        return document;
    }

    default void writeBySaveBatch(Collection<?> sourceObjCollection, List<Document> documentList){
        sourceObjCollection.forEach(sourceObj -> {
            Document document = new Document();
            writeBySave(sourceObj,document);
            documentList.add(document);
        });
    }

    default List<Document> writeBySaveBatch(Collection<?> sourceObjCollection){
        return new ArrayList<Document>(){{
            sourceObjCollection.forEach(sourceObj -> {
                Document document = new Document();
                writeBySave(sourceObj,document);
                add(document);
            });
        }};
    }

    void writeByUpdate(Object sourceObj, Document document);

    default Document writeByUpdate(Object sourceObj){
        Document document = new Document();
        writeByUpdate(sourceObj,document);
        return document;
    }

    default void writeByUpdateBatch(Collection<?> sourceObjCollection, List<Document> documentList){
        sourceObjCollection.forEach(sourceObj -> {
            Document document = new Document();
            writeByUpdate(sourceObj,document);
            documentList.add(document);
        });
    }

    default List<Document> writeByUpdateBatch(Collection<?> sourceObjCollection){
        List<Document> documentList = new ArrayList<>();
        sourceObjCollection.forEach(sourceObj -> {
            Document document = new Document();
            writeByUpdate(sourceObj,document);
            documentList.add(document);
        });
        return documentList;
    }

    <T> T readInternal(Document document, Class<T> clazz);

    default <T> List<T> read(MongoIterable<Document> findIterable, Class<T> clazz){
        List<T> resultList = new ArrayList<>();
        try (MongoCursor<Document> mongoCursor = findIterable.iterator()) {
            if (mongoCursor.hasNext()){
                resultList.add(read(mongoCursor.next(), clazz));
            }
        }
        return resultList;
    }

}
