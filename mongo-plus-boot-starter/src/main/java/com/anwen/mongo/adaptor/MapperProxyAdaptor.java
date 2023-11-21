package com.anwen.mongo.adaptor;

import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.proxy.MapperProxy;
import org.springframework.beans.factory.FactoryBean;

/**
 * @Description: core的代理类与Spring之间的适配器
 * @Name: MapperProxyAdaptor
 * @Author: Bomber
 * @CreateTime: 2023/11/21 14:31
 */
public class MapperProxyAdaptor<M extends BaseMapper<T>, T> extends MapperProxy<M, T> implements FactoryBean<M> {

    @Override
    public boolean isSingleton() {
        return true;
    }

}
