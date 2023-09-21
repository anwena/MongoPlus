package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.enums.CollectionNameConvertEnum;

import java.util.Optional;

public class MongoCollectionUtils {

	private MongoCollectionUtils() {
	}

	/**
	 * 根据实体类类型构建collection name
	 * 取类名简单名称的第一个字母小写
	 * @param clazz 类
	 * @return mongo collection name
	 */
	public static String getFirstLowerCaseName(Class<?> clazz) {
		return Optional.ofNullable(getByAnnotation(clazz))
				.orElse(StringUtils.firstCharToLowerCase(clazz.getSimpleName()));
	}

	/**
	 * 根据实体类类型构建collection name
	 * 取类的简单名称全小写
	 * @param clazz 类
	 * @return mongo collection name
	 */
	public static String getLowerCaseName(Class<?> clazz) {
		return Optional.ofNullable(getByAnnotation(clazz))
				.orElse(clazz.getSimpleName().toLowerCase());
	}

	/**
	 * 根据实体类类型构建collection name
	 * 取类的简单名称
	 * @param clazz 类
	 * @return mongo collection name
	 */
	public static String getSimpleClassName(Class<?> clazz) {
		return Optional.ofNullable(getByAnnotation(clazz))
				.orElse(clazz.getSimpleName());
	}
	/**
	 * 根据实体类类型构建collection name
	 * 取类的简单名称并按驼峰转下划线
	 * @param clazz 类
	 * @return mongo collection name
	 */
	public static String getUnderlineClassName(Class<?> clazz) {
		return Optional.ofNullable(getByAnnotation(clazz))
				.orElse(StringUtils.camelToUnderline(clazz.getSimpleName()));
	}

	private static String getByAnnotation(Class<?> clazz) {
		if (clazz.isAnnotationPresent(CollectionName.class)) {
			return clazz.getAnnotation(CollectionName.class).value();
		}
		return null;
	}

	public static CollectionNameConvert build(CollectionNameConvertEnum mappingStrategy) {
		switch (mappingStrategy) {
			case ALL_CHAR_LOWERCASE: return MongoCollectionUtils::getLowerCaseName;
			case FIRST_CHAR_LOWERCASE: return MongoCollectionUtils::getFirstLowerCaseName;
			case CLASS_NAME: return MongoCollectionUtils::getSimpleClassName;
			case CAMEL_TO_UNDERLINE: return MongoCollectionUtils::getUnderlineClassName;
		}
		return MongoCollectionUtils::getLowerCaseName;
	}
}
