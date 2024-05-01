/*
 *    Copyright 2009-2024 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.anwen.mongo.logging;

import com.anwen.mongo.logging.commons.JakartaCommonsLoggingImpl;
import com.anwen.mongo.logging.jdk14.Jdk14LoggingImpl;
import com.anwen.mongo.logging.log4j.Log4jImpl;
import com.anwen.mongo.logging.log4j2.Log4j2Impl;
import com.anwen.mongo.logging.nologging.NoLoggingImpl;
import com.anwen.mongo.logging.slf4j.Slf4jImpl;
import com.anwen.mongo.logging.stdout.StdOutImpl;

import java.lang.reflect.Constructor;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Clinton Begin
 * @author Eduardo Macarron
 */
public final class LogFactory {

  /**
   * Marker to be used by logging implementations that support markers.
   */
  public static final String MARKER = "MYBATIS";

  private static final ReentrantLock lock = new ReentrantLock();
  private static Constructor<? extends Log> logConstructor;

  static {
    tryImplementation(LogFactory::useSlf4jLogging);
    tryImplementation(LogFactory::useCommonsLogging);
    tryImplementation(LogFactory::useLog4J2Logging);
    tryImplementation(LogFactory::useLog4JLogging);
    tryImplementation(LogFactory::useJdkLogging);
    tryImplementation(LogFactory::useNoLogging);
  }

  private LogFactory() {
    // disable construction
  }

  public static Log getLog(Class<?> clazz) {
    return getLog(clazz.getName());
  }

  public static Log getLog(String logger) {
    try {
      return logConstructor.newInstance(logger);
    } catch (Throwable t) {
      throw new LogException("Error creating logger for logger " + logger + ".  Cause: " + t, t);
    }
  }

  public static void useCustomLogging(Class<? extends Log> clazz) {
    setImplementation(clazz);
  }

  public static void useSlf4jLogging() {
    setImplementation(Slf4jImpl.class);
  }

  public static void useCommonsLogging() {
    setImplementation(JakartaCommonsLoggingImpl.class);
  }

  /**
   * @deprecated Since 3.5.9 - See https://github.com/mybatis/mybatis-3/issues/1223. This method will remove future.
   */
  @Deprecated
  public static void useLog4JLogging() {
    setImplementation(Log4jImpl.class);
  }

  public static void useLog4J2Logging() {
    setImplementation(Log4j2Impl.class);
  }

  public static void useJdkLogging() {
    setImplementation(Jdk14LoggingImpl.class);
  }

  public static void useStdOutLogging() {
    setImplementation(StdOutImpl.class);
  }

  public static void useNoLogging() {
    setImplementation(NoLoggingImpl.class);
  }

  private static void tryImplementation(Runnable runnable) {
    if (logConstructor == null) {
      try {
        runnable.run();
      } catch (Throwable t) {
        // ignore
      }
    }
  }

  private static void setImplementation(Class<? extends Log> implClass) {
    lock.lock();
    try {
      Constructor<? extends Log> candidate = implClass.getConstructor(String.class);
      Log log = candidate.newInstance(LogFactory.class.getName());
      if (log.isDebugEnabled()) {
        log.debug("Logging initialized using '" + implClass + "' adapter.");
      }
      logConstructor = candidate;
    } catch (Throwable t) {
      throw new LogException("Error setting Log implementation.  Cause: " + t, t);
    } finally {
      lock.unlock();
    }
  }

}
