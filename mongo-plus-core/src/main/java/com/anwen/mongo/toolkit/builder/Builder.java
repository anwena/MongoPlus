package com.anwen.mongo.toolkit.builder;

/**
 * 生成器接口用于在生成器设计模式中将类指定为生成器对象。构建器能够创建和配置对象或结果，这些对象或结果通常需要多个步骤才能构建，或者派生起来非常复杂。
 * 生成器接口定义了类必须实现的单个方法build（）。此方法的结果应该是执行所有构建操作后的最终配置对象或结果。
 * 建议使用为配置正在构建的对象或结果而提供的方法返回对此的引用，以便将方法调用链接在一起。
 * @author JiaChaoYang
 * @date 2024/3/16 22:41
*/
public interface Builder<T> {

    /**
     * 返回对构建器正在构建的对象或正在计算的结果的引用。
     * @author JiaChaoYang
     * @date 2024/3/16 22:41
    */
    T build();

}
