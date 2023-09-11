package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.collection.CollectionName;

import java.util.Optional;

public class MongoCollectionUtils {

	private MongoCollectionUtils() {
	}


	/**
	 * 根据实体类类型构建collection name
	 * 策略：优先从注解取，否则取类名简单名称的第一个字母小写
	 * @param clazz 类
	 * @return mongo collection name
	 */
	public static String getFirstLowerCaseName(Class<?> clazz) {
		return Optional.ofNullable(getByAnnotation(clazz))
				.orElse(StringUtils.firstCharToLowerCase(clazz.getSimpleName()));
	}

	/**
	 * 根据实体类类型构建collection name
	 * 策略：优先从注解取，否则取类的简单名称全小写
	 * @param clazz 类
	 * @return mongo collection name
	 */
	public static String getLowerCaseName(Class<?> clazz) {
		return Optional.ofNullable(getByAnnotation(clazz))
				.orElse(StringUtils.firstCharToLowerCase(clazz.getSimpleName().toLowerCase()));
	}

	private static String getByAnnotation(Class<?> clazz) {
		if (clazz.isAnnotationPresent(CollectionName.class)) {
			return clazz.getAnnotation(CollectionName.class).value();
		}
		return null;
	}

}
