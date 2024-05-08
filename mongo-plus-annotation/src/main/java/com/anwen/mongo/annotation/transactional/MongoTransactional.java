package com.anwen.mongo.annotation.transactional;

import com.anwen.mongo.enums.ReadConcernEnum;
import com.anwen.mongo.enums.WriteConcernEnum;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.TimeUnit;

/**
 * Mongo事务注解
 *
 * @author JiaChaoYang
 **/
@Target(ElementType.METHOD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface MongoTransactional {

    /*clientSession选项，{@link com.mongodb.client.ClientSessionOptions}类*/

    /**
     * 使用会话的操作是否应该彼此一致。
     *
     * @author JiaChaoYang
     * @date 2024/5/2 下午4:37
     */
    boolean causallyConsistent() default true;

    /**
     * 使用此会话的读取操作是否应全部共享同一个快照。
     *
     * @author JiaChaoYang
     * @date 2024/5/2 下午4:42
     */
    boolean snapshot() default false;

    /*Transaction选项，{@link com.mongodb.client.TransactionOptions}类*/

    /**
     * 设置一致性读策略
     *
     * @author JiaChaoYang
     * @date 2024/5/2 下午4:46
     */
    ReadConcernEnum readConcern() default ReadConcernEnum.DEFAULT;

    /**
     * 设置写入安全的级别
     *
     * @author anwen
     * @date 2024/5/2 下午5:26
     */
    WriteConcernEnum writeConcern() default WriteConcernEnum.ACKNOWLEDGED;

    /**
     * 用于指定事务提交的最大时间限制以防止因为某些原因导致事务长时间无法提交。如果事务在指定的时间内没有提交，MongODB 会自动中止 (abort) 事务。
     */
    long maxCommitTimeMS() default 0;

    /**
     * 时间单位
     */
    TimeUnit timeUnit() default TimeUnit.MILLISECONDS;

    /**
     * 长度为1则有效
     * 配置读取偏好设置
     */
    MongoReadPreference[] preference() default {};

    /**
     * 捕获该异常并进行回滚
     *
     * @return 需要回滚的异常
     */
    Class<? extends Throwable>[] rollbackFor() default {};

    /**
     * 捕获该异常不进行回滚
     *
     * @return 不需要回滚的异常
     */
    Class<? extends Throwable>[] noRollbackFor() default {};

}
