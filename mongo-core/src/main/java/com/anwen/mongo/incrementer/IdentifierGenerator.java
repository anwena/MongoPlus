/*
 * Copyright (c) 2011-2023, baomidou (jobob@qq.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.anwen.mongo.incrementer;

import com.anwen.mongo.id.IdWorker;
import com.anwen.mongo.toolkit.StringUtils;

/**
 * Id生成器接口
 * @reference mp
 * @author JiaChaoYang
 * @date 2023/8/12 0:34
*/
public interface IdentifierGenerator {

    /**
     * 判断是否分配 ID
     *
     * @param idValue 主键值
     * @return true 分配 false 无需分配
     */
    default boolean assignId(Object idValue) {
        return StringUtils.checkValNull(idValue);
    }

    /**
     * 生成Id
     *
     * @param entity 实体
     * @return id
     */
    Number nextId(Object entity);

    /**
     * 生成uuid
     *
     * @param entity 实体
     * @return uuid
     */
    default String nextUUID(Object entity) {
        return IdWorker.get32UUID();
    }
}
