package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import com.anwen.mongo.support.SFunction;
import com.mongodb.lang.Nullable;

/**
 * unwind管道可选项
 *
 * @author anwen
 * @date 2024/6/16 下午9:05
 */
public class UnwindOptions {

    private Boolean preserveNullAndEmptyArrays;
    private String includeArrayIndex;

    /**
     * 如果为 true，则展开阶段将包含具有空值或空数组的文档
     *
     * @return 保留空值和空数组值或 null
     */
    @Nullable
    public Boolean isPreserveNullAndEmptyArrays() {
        return preserveNullAndEmptyArrays;
    }

    /**
     * 如果展开阶段应包含具有空值或空数组的文档，则设置为 true
     * @param preserveNullAndEmptyArrays 描述展开阶段是否应包含具有空值或空数组的文档的标志
     * @author anwen
     * @date 2024/6/16 下午9:06
     */
    public UnwindOptions preserveNullAndEmptyArrays(@Nullable final Boolean preserveNullAndEmptyArrays) {
        this.preserveNullAndEmptyArrays = preserveNullAndEmptyArrays;
        return this;
    }

    /**
     * Gets the includeArrayIndex field if set or null
     *
     * @return the includeArrayIndex field if set or null
     */
    @Nullable
    public String getIncludeArrayIndex() {
        return includeArrayIndex;
    }

    /**
     * 设置用于存储展开项的数组索引的字段
     *
     * @param arrayIndexFieldName 用于存储展开项的数组索引的字段
     * @return this
     */
    public UnwindOptions includeArrayIndex(@Nullable final String arrayIndexFieldName) {
        this.includeArrayIndex = arrayIndexFieldName;
        return this;
    }

    /**
     * 设置用于存储展开项的数组索引的字段
     *
     * @param arrayIndexFieldName 用于存储展开项的数组索引的字段
     * @return this
     */
    public <T> UnwindOptions includeArrayIndex(@Nullable final SFunction<T,?> arrayIndexFieldName) {
        if (arrayIndexFieldName != null) {
            this.includeArrayIndex = arrayIndexFieldName.getFieldNameLine();
        }
        return this;
    }

    @Override
    public String toString() {
        return "UnwindOptions{"
                + "preserveNullAndEmptyArrays=" + preserveNullAndEmptyArrays
                + ", includeArrayIndex='" + includeArrayIndex + '\''
                + '}';
    }

}
