package com.anwen.mongo.toolkit;

import com.anwen.mongo.sql.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.sql.update.LambdaUpdateMongoWrapper;

/**
 * 快速构建链式调用
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:27
*/ 
public final class ChainWrappers {

    public static <T> LambdaQueryChainWrapper<T> lambdaQueryChain(SqlOperation<T> sqlOperation){
        return new LambdaQueryChainWrapper<>(sqlOperation);
    }

    public static <T> LambdaUpdateChainWrapper<T> lambdaUpdateChain(SqlOperation<T> sqlOperation){
        return new LambdaUpdateChainWrapper<>(sqlOperation);
    }

}
