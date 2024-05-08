package com.anwen.mongo.logic.interceptor;

import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.logic.LogicDeleteHandler;
import com.anwen.mongo.model.LogicDeleteResult;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.InsertOneModel;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;

import java.util.List;
import java.util.Objects;

/**
 * 逻辑删除默认字段拦截器(初始化逻辑未删除字段、建议方案：使用数据库默认字段 > 其次是手动设置 > 配置框架提供拦截器 > 自定义拦截器）)
 *
 * @author loser
 * @date 2024/4/30
 */
public class LogicAutoFillInterceptor implements Interceptor {

    @Override
    public List<Document> executeSave(List<Document> documentList, MongoCollection<Document> collection) {

        Class<?> clazz = LogicDeleteHandler.getBeanClass(collection);
        if (Objects.isNull(clazz)) {
            return documentList;
        }
        LogicDeleteResult result = LogicDeleteHandler.mapper().get(clazz);
        if (Objects.nonNull(result)) {
            for (Document document : documentList) {
                if (!document.containsKey(result.getColumn())) {
                    document.put(result.getColumn(), result.getLogicNotDeleteValue());
                }
            }
        }
        return documentList;

    }

    @Override
    public List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection) {

        Class<?> clazz = LogicDeleteHandler.getBeanClass(collection);
        if (Objects.isNull(clazz)) {
            return writeModelList;
        }
        for (WriteModel<Document> documentWriteModel : writeModelList) {
            if (documentWriteModel instanceof InsertOneModel) {
                Document document = ((InsertOneModel<Document>) documentWriteModel).getDocument();
                LogicDeleteResult result = LogicDeleteHandler.mapper().get(clazz);
                if (Objects.nonNull(result)) {
                    if (!document.containsKey(result.getColumn())) {
                        document.put(result.getColumn(), result.getLogicNotDeleteValue());
                    }
                }
            }
        }
        return writeModelList;

    }

}
