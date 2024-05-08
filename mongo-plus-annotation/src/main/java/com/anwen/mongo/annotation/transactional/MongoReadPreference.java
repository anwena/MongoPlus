package com.anwen.mongo.annotation.transactional;

import com.anwen.mongo.enums.ReadPreferenceEnum;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.TimeUnit;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface MongoReadPreference {

    ReadPreferenceEnum preferenceEnum() default ReadPreferenceEnum.PRIMARY;

    long maxStaleness() default 0;

    TimeUnit timeUnit() default TimeUnit.MILLISECONDS;

}
