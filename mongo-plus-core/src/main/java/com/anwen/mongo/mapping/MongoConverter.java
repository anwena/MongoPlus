package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoIterable;
import org.bson.Document;
import org.bson.types.ObjectId;

import java.lang.reflect.Field;
import java.util.*;

import static com.anwen.mongo.convert.Converter.convertKeysToCamelCase;

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

    /**
     * 添加的映射器
     * @author JiaChaoYang
     * @date 2024/5/1 下午11:52
     */
    default Document writeBySave(Object sourceObj){
        Document document = new Document();
        writeBySave(sourceObj,document);
        return document;
    }

    /**
     * map映射到document
     * @author anwen
     * @date 2024/5/28 下午8:35
     */
    default Document write(Map<String,Object> map){
        Document document = new Document();
        write(map,document);
        return document;
    }

    /**
     * 批量映射
     * @author anwen
     * @date 2024/5/28 下午8:35
     */
    default List<Document> writeBatch(Collection<Map<String,Object>> sourceObjCollection, List<Document> documentList){
        sourceObjCollection.forEach(sourceObj -> {
            documentList.add(write(sourceObj));
        });
        return documentList;
    }

    /**
     * 批量映射
     * @author anwen
     * @date 2024/5/28 下午8:35
     */
    default List<Document> writeBatch(Collection<Map<String,Object>> sourceObjCollection){
        return new ArrayList<Document>(){{
            sourceObjCollection.forEach(sourceObj -> {
                add(write(sourceObj));
            });
        }};
    }

    /**
     * 批量映射
     * @author anwen
     * @date 2024/5/28 下午8:35
     */
    default void writeBySaveBatch(Collection<?> sourceObjCollection, List<Document> documentList){
        sourceObjCollection.forEach(sourceObj -> {
            Document document = new Document();
            writeBySave(sourceObj,document);
            documentList.add(document);
        });
    }

    /**
     * 批量映射
     * @author anwen
     * @date 2024/5/28 下午8:35
     */
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

    /**
     * 写内部属性
     * @author anwen
     * @date 2024/6/28 上午12:46
     */
    default <T> T readInternal(Document document, Class<T> clazz){
        return readInternal(document,new TypeReference<T>(clazz){});
    }

    /**
     * 写内部属性
     * @author anwen
     * @date 2024/6/28 上午12:46
     */
    <T> T readInternal(Object sourceObj, TypeReference<T> typeReference);

    /**
     * 写为Class
     * @author anwen
     * @date 2024/5/28 下午8:37
     */
    default <T> List<T> read(MongoIterable<Document> findIterable, Class<T> clazz) {
        return new ArrayList<T>(){{
            findIterable.forEach(document -> add(convertDocument(document,clazz)));
        }};
    }

    /**
     * 写为class，根据传入的type
     * @author anwen
     * @date 2024/5/28 下午8:37
     */
    default <T> List<T> read(MongoIterable<Document> findIterable, TypeReference<T> typeReference){
        return new ArrayList<T>(){{
            findIterable.forEach(document -> add(read(document, typeReference)));
        }};
    }

    /**
     * 写为class
     * @author anwen
     * @date 2024/5/28 下午8:37
     */
    @SuppressWarnings("unchecked")
    default <T> T readDocument(MongoIterable<Document> findIterable,Class<?> clazz){
        Document document = findIterable.first();
        if (document != null){
            return (T) convertDocument(document, clazz);
        }
        return null;
    }

    /**
     * 写为class
     * @author anwen
     * @date 2024/5/28 下午8:37
     */
    default <T> T readDocument(MongoIterable<Document> findIterable,TypeReference<T> typeReference){
        Document document = findIterable.first();
        if (document != null){
            return read(document, typeReference);
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    default  <T> T convertDocument(Document document, Class<T> clazz) {
        if (ClassTypeUtil.isTargetClass(Map.class,clazz)) {
            return (T) convertKeysToCamelCase(document);
        } else {
            return read(document, clazz);
        }
    }

    @SuppressWarnings({"unchecked","rawtypes"})
    default void reSetIdValue(Object sourceObj, Document document) {
        if (Objects.isNull(sourceObj) || Objects.isNull(document) || !document.containsKey(SqlOperationConstant._ID)) {
            return;
        }
        // Map类型不需要再做下边的操作 因为它们只针对实体类
        if (ClassTypeUtil.isTargetClass(Map.class,sourceObj.getClass())){
            Map map = (Map) sourceObj;
            if (!map.containsKey(SqlOperationConstant._ID)){
                map.put(SqlOperationConstant._ID, document.get(SqlOperationConstant._ID));
            }
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
