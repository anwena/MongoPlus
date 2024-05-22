package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoIterable;
import org.bson.Document;
import org.bson.types.ObjectId;

import java.lang.reflect.Field;
import java.util.*;

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

    default List<Document> writeBatch(Collection<Map<String,Object>> sourceObjCollection, List<Document> documentList){
        sourceObjCollection.forEach(sourceObj -> {
            documentList.add(write(sourceObj));
        });
        return documentList;
    }

    default List<Document> writeBatch(Collection<Map<String,Object>> sourceObjCollection){
        return new ArrayList<Document>(){{
            sourceObjCollection.forEach(sourceObj -> {
                add(write(sourceObj));
            });
        }};
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
            while (mongoCursor.hasNext()){
                resultList.add(read(mongoCursor.next(), clazz));
            }
        }
        return resultList;
    }

    @SuppressWarnings("unchecked")
    default <T> T readDocument(MongoIterable<Document> findIterable,Class<?> clazz){
        try (MongoCursor<Document> mongoCursor = findIterable.iterator()) {
            if (mongoCursor.hasNext()){
                return (T)read(mongoCursor.next(), clazz);
            }
        }
        return null;
    }

    default void reSetIdValue(Object sourceObj, Document document) {
        if (Objects.isNull(sourceObj) || Objects.isNull(document) || !document.containsKey(SqlOperationConstant._ID)) {
            return;
        }
        TypeInformation typeInformation = TypeInformation.of(sourceObj);
        FieldInformation idFieldInformation = typeInformation.getAnnotationField(ID.class, "@ID field not found");
        Object idValue = idFieldInformation.getValue();
        if (Objects.isNull(idValue)) {
            Object idV = document.get(SqlOperationConstant._ID);
            Field field = idFieldInformation.getField();
            field.setAccessible(true);
            try {
                if (idV instanceof ObjectId) {
                    field.set(sourceObj, idV.toString());
                } else {
                    field.set(sourceObj, idV);
                }
            } catch (Exception e) {
                throw new MongoPlusFieldException("reSet id value error", e);
            }
        }
    }

    default <T> void batchReSetIdValue(Collection<T> entityList, List<Document> documentList) {
        int index = 0;
        for (T t : entityList) {
            Document document = documentList.get(index);
            reSetIdValue(t, document);
            index++;
        }
    }

}
