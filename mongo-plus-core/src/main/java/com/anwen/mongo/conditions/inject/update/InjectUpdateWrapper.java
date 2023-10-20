package com.anwen.mongo.conditions.inject.update;

import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.toolkit.ChainWrappers;

import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class InjectUpdateWrapper extends UpdateChainWrapper<Map<String,Object>,InjectUpdateWrapper> {

    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/12 2:14
     */
    public UpdateChainWrapper<Map<String,Object>, InjectUpdateWrapper> lambdaUpdate(){
        return ChainWrappers.lambdaUpdateChainInject();
    }

}
