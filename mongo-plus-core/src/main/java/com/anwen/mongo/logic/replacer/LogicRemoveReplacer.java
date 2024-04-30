package com.anwen.mongo.logic.replacer;

import com.anwen.mongo.cache.global.ClassLogicDeleteCache;
import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.logic.LogicDeleteHandler;
import com.anwen.mongo.model.LogicDeleteResult;
import com.anwen.mongo.replacer.Replacer;
import com.anwen.mongo.support.BoolFunction;
import com.mongodb.BasicDBObject;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.lang.reflect.Method;
import java.util.Objects;

/**
 * 逻辑删除替换器
 *
 * @author loser
 * @date 2024/4/30
 */
public class LogicRemoveReplacer implements Replacer {

    @Override
    public Object invoke(Object proxy, Object target, Method method, Object[] args) throws Throwable {

        Class<?> clazz = LogicDeleteHandler.getBeanClass((MongoCollection<Document>) args[1]);
        if (Objects.isNull(clazz)) {
            return method.invoke(target, args);
        }
        LogicDeleteResult result = LogicDeleteHandler.mapper().get(clazz);
        if (Objects.isNull(result)) {
            return method.invoke(target, args);
        }
        Method updateMethod = target.getClass().getMethod(ExecuteMethodEnum.UPDATE.getMethod(), Bson.class, Bson.class, MongoCollection.class);
        Document updateBasic = new Document(result.getColumn(), result.getLogicDeleteValue());
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), updateBasic);
        Object[] updateArgs = new Object[]{args[0], update, args[1]};
        UpdateResult res = (UpdateResult) updateMethod.invoke(target, updateArgs);
        return new DeleteResult() {
            @Override
            public boolean wasAcknowledged() {
                return res.wasAcknowledged();
            }

            @Override
            public long getDeletedCount() {
                return res.getModifiedCount();
            }
        };

    }

    @Override
    public BoolFunction supplier() {
        return (proxy, target, method, args) -> ClassLogicDeleteCache.open && method.getName().equals(ExecuteMethodEnum.REMOVE.getMethod());
    }

}
