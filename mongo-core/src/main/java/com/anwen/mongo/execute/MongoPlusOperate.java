package com.anwen.mongo.execute;

import com.anwen.mongo.conditions.interfaces.Inject.InjectQuery;
import com.anwen.mongo.mapper.MongoPlusMapMapper;

import java.util.Map;

/**
 * @author JiaChaoYang0
 * @project mongo
 * @description 无实体类操作，统一返回map
 * @date 2023-07-20 21:54
 **/
@Deprecated
public class MongoPlusOperate extends MongoPlusMapMapper implements InjectQuery {

    public MongoPlusOperate(SqlOperation<Map<String, Object>> sqlOperation) {
        super(sqlOperation);
    }
}
