package com.anwen.mongo.interceptor.business;

import com.anwen.mongo.domain.MongoPlusException;
import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.model.MutablePair;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.InsertOneModel;
import com.mongodb.client.model.UpdateManyModel;
import com.mongodb.client.model.WriteModel;
import org.bson.BsonArray;
import org.bson.BsonDocument;
import org.bson.BsonValue;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.ArrayList;
import java.util.List;

/**
 * 数据变动记录拦截器
 *
 * @author anwen
 * @date 2024/6/27 下午5:01
 * @since by mybatis-plus
 */
@SuppressWarnings("unchecked")
public class DataChangeRecorderInnerInterceptor implements Interceptor {

    private final Log log = LogFactory.getLog(DataChangeRecorderInnerInterceptor.class);

    /**
     * 超出阈值提示信息
     *
     * @date 2024/6/27 下午5:48
     */
    private String exceptionMessage = "The operation has exceeded the security threshold and has been intercepted!";

    /**
     * 忽略的表
     *
     * @date 2024/6/27 下午5:33
     */
    private List<String> ignoredColumnList = new ArrayList<>();

    /**
     * 批量更新条数上限
     *
     * @date 2024/6/27 下午5:32
     */
    private Integer batchUpdateLimit = 1000;

    /**
     * 是否显示完整数据，开启后，changedData字段数据量可能会很大
     * 默认不开启，只显示数量
     * @date 2024/6/27 下午8:14
     */
    private Boolean displayCompleteData = true;

    @Override
    public Object[] beforeExecute(ExecuteMethodEnum executeMethodEnum, Object[] source, MongoCollection<Document> collection) {
        if (ignoredColumnList.contains(collection.getNamespace().getCollectionName())){
            return source;
        }
        long startTs = System.currentTimeMillis();
        OperationResult operationResult = null;
        if (executeMethodEnum == ExecuteMethodEnum.SAVE) {
            operationResult = processSave(source);
        } else if (executeMethodEnum == ExecuteMethodEnum.UPDATE) {
            operationResult = processUpdate(source);
        } else if (executeMethodEnum == ExecuteMethodEnum.REMOVE) {
            operationResult = processRemove(source);
        } else if (executeMethodEnum == ExecuteMethodEnum.BULK_WRITE) {
            operationResult = processBulkWrite(source);
        }
        if (operationResult != null) {
            operationResult.setCollectionName(collection.getNamespace().getCollectionName());
            operationResult.setRecordStatus(true);
            long costThis = System.currentTimeMillis() - startTs;
            operationResult.setCost(costThis);
            log.info(String.format("%s DataChangeRecord: %s",executeMethodEnum.name(), operationResult));
        }
        return source;
    }

    private OperationResult processSave(Object[] source) throws DataUpdateLimitationException {
        OperationResult operationResult = new OperationResult();
        List<Document> documentList = (List<Document>) source[0];
        if (documentList.size() > batchUpdateLimit) {
            log.error("batch save limit exceed: count={}, BATCH_UPDATE_LIMIT={}",documentList.size(), batchUpdateLimit);
            throw new DataUpdateLimitationException(exceptionMessage);
        }
        operationResult.setOperation(ExecuteMethodEnum.SAVE.name());
        operationResult.setChangedData(displayCompleteData ? documentList.toString() : String.valueOf(documentList.size()));
        return operationResult;
    }

    private OperationResult processUpdate(Object[] source) throws DataUpdateLimitationException {
        OperationResult operationResult = new OperationResult();
        List<MutablePair<Bson, Bson>> documentList = (List<MutablePair<Bson, Bson>>) source[0];
        if (documentList.size() > batchUpdateLimit) {
            log.error("batch update limit exceed: count={}, BATCH_UPDATE_LIMIT={}",documentList.size(),batchUpdateLimit);
            throw new DataUpdateLimitationException(exceptionMessage);
        }
        operationResult.setOperation(ExecuteMethodEnum.UPDATE.name());
        List<String> dataList = new ArrayList<>();
        documentList.forEach(mutablePair -> {
            String left = mutablePair.getRight().toBsonDocument().toString();
            String right = mutablePair.getRight().toBsonDocument().toString();
            dataList.add("(left="+left+",right="+right+")");
        });
        operationResult.setChangedData(displayCompleteData ? dataList.toString() : String.valueOf(documentList.size()));
        return operationResult;
    }

    private OperationResult processRemove(Object[] source) throws DataUpdateLimitationException {
        OperationResult operationResult = new OperationResult();
        Bson bson = (Bson) source[0];
        BsonDocument bsonDocument = bson.toBsonDocument();
        bsonDocument.forEach((k, v) -> {
            if (v.isDocument()) {
                BsonDocument document = v.asDocument();
                if (document.containsKey(SpecialConditionEnum.IN.getCondition())) {
                    BsonValue bsonValue = document.get(SpecialConditionEnum.IN.getCondition());
                    BsonArray inArray = bsonValue.asArray();
                    if (inArray.size() > batchUpdateLimit) {
                        log.error("batch remove limit exceed: count={}, BATCH_UPDATE_LIMIT={}",inArray.size(), batchUpdateLimit);
                        throw new DataUpdateLimitationException(exceptionMessage);
                    }
                }
            }
        });
        operationResult.setOperation(ExecuteMethodEnum.REMOVE.name());
        operationResult.setChangedData(displayCompleteData ? bsonDocument.toString() : String.valueOf(bsonDocument.size()));
        return operationResult;
    }

    private OperationResult processBulkWrite(Object[] source) {
        OperationResult operationResult = new OperationResult();
        List<WriteModel<Document>> writeModelList = (List<WriteModel<Document>>) source[0];
        long insertCount = writeModelList.stream().filter(writeModel -> writeModel instanceof InsertOneModel).count();
        long updateCount = writeModelList.stream().filter(writeModel -> writeModel instanceof UpdateManyModel).count();
        if (insertCount > batchUpdateLimit || updateCount > batchUpdateLimit) {
            log.error("batch bulkWrite limit exceed: count={}, BATCH_UPDATE_LIMIT={}",insertCount, batchUpdateLimit);
            throw new DataUpdateLimitationException(exceptionMessage);
        }
        operationResult.setOperation(ExecuteMethodEnum.BULK_WRITE.name());
        String changedData = String.valueOf(writeModelList.size());
        if (displayCompleteData) {
            List<String> dataList = new ArrayList<>();
            for (WriteModel<Document> writeModel : writeModelList) {
                if (writeModel instanceof InsertOneModel) {
                    dataList.add(((InsertOneModel<Document>) writeModel).toString());
                } else if (writeModel instanceof UpdateManyModel) {
                    UpdateManyModel<Document> updateManyModel = (UpdateManyModel<Document>) writeModel;
                    String updateManyModelString = "UpdateManyModel{"
                            + "filter=" + updateManyModel.getFilter()
                            + ", update=" + (updateManyModel.getUpdate() != null ? updateManyModel.getUpdate().toBsonDocument().toString() : updateManyModel.getUpdatePipeline())
                            + ", options=" + updateManyModel.getUpdatePipeline()
                            + '}';
                    dataList.add(updateManyModelString);
                }
            }
            changedData = dataList.toString();
        }
        operationResult.setChangedData(changedData);
        return operationResult;
    }

    public Boolean getDisplayCompleteData() {
        return displayCompleteData;
    }

    public void setDisplayCompleteData(Boolean displayCompleteData) {
        this.displayCompleteData = displayCompleteData;
    }

    public String getExceptionMessage() {
        return exceptionMessage;
    }

    public void setExceptionMessage(String exceptionMessage) {
        this.exceptionMessage = exceptionMessage;
    }

    public List<String> getIgnoredColumnList() {
        return ignoredColumnList;
    }

    public void setIgnoredColumnList(List<String> ignoredColumnList) {
        this.ignoredColumnList = ignoredColumnList;
    }

    public Integer getBatchUpdateLimit() {
        return batchUpdateLimit;
    }

    public void setBatchUpdateLimit(Integer batchUpdateLimit) {
        this.batchUpdateLimit = batchUpdateLimit;
    }

    public static class OperationResult {
        /**
         * 操作类型
         *
         * @date 2024/6/27 下午5:37
         */
        private String operation;

        /**
         * 记录状态
         *
         * @date 2024/6/27 下午5:37
         */
        private boolean recordStatus;

        /**
         * 数据源名称
         *
         * @date 2024/7/9 下午5:05
         */
        private String datasourceName;

        /**
         * 数据库名
         *
         * @date 2024/6/27 下午5:37
         */
        private String databaseName;

        /**
         * 集合名
         *
         * @date 2024/6/27 下午5:37
         */
        private String collectionName;

        /**
         * 改动数据
         *
         * @date 2024/6/27 下午5:37
         */
        private String changedData;

        /**
         * 插件耗时
         *
         * @date 2024/6/27 下午5:38
         */
        private long cost;

        public String getOperation() {
            return operation;
        }

        public void setOperation(String operation) {
            this.operation = operation;
        }

        public boolean isRecordStatus() {
            return recordStatus;
        }

        public void setRecordStatus(boolean recordStatus) {
            this.recordStatus = recordStatus;
        }

        public String getDatasourceName() {
            return datasourceName;
        }

        public void setDatasourceName(String datasourceName) {
            this.datasourceName = datasourceName;
        }

        public String getDatabaseName() {
            return databaseName;
        }

        public void setDatabaseName(String databaseName) {
            this.databaseName = databaseName;
        }

        public String getCollectionName() {
            return collectionName;
        }

        public void setCollectionName(String collectionName) {
            this.collectionName = collectionName;
        }

        public String getChangedData() {
            return changedData;
        }

        public void setChangedData(String changedData) {
            this.changedData = changedData;
        }

        public long getCost() {
            return cost;
        }

        public void setCost(long cost) {
            this.cost = cost;
        }
        @Override
        public String toString() {
            return "{" +
                    "\"datasourceName\":\"" + datasourceName + "\"," +
                    "\"databaseName\":\"" + databaseName + "\"," +
                    "\"collectionName\":\"" + collectionName + "\"," +
                    "\"operation\":\"" + operation + "\"," +
                    "\"recordStatus\":\"" + recordStatus + "\"," +
                    "\"cost(ms)\":" + cost + "," +
                    "\"changedData\":" + changedData + "}";
        }
    }

    public static class DataUpdateLimitationException extends MongoPlusException {

        public DataUpdateLimitationException(String message) {
            super(message);
        }

    }

}
