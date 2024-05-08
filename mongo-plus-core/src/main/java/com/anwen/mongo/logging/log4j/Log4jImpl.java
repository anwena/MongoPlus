/*
 *    Copyright 2009-2023 the original author or authors.
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
package com.anwen.mongo.logging.log4j;

import com.anwen.mongo.logging.Log;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * @author Eduardo Macarron
 *
 * @deprecated Since 3.5.9 - See https://github.com/mybatis/mybatis-3/issues/1223. This class will remove future.
 */
@Deprecated
public class Log4jImpl implements Log {

  private static final String FQCN = Log4jImpl.class.getName();

  private final Logger log;

  public Log4jImpl(String clazz) {
    log = Logger.getLogger(clazz);
  }

  @Override
  public boolean isDebugEnabled() {
    return log.isDebugEnabled();
  }

  @Override
  public boolean isTraceEnabled() {
    return log.isTraceEnabled();
  }

  @Override
  public void info(String s) {
    log.log(FQCN, Level.INFO, s, null);
  }

  @Override
  public void error(String s, Throwable e) {
    log.log(FQCN, Level.ERROR, s, e);
  }

  @Override
  public void error(String s, Object arg) {
    log.log(FQCN, Level.ERROR, s, (Throwable) arg);
  }

  @Override
  public void error(String s, Object arg1, Object arg2) {
    log.log(FQCN, Level.ERROR, s, (Throwable) arg2);
  }

  @Override
  public void error(String s) {
    log.log(FQCN, Level.ERROR, s, null);
  }

  @Override
  public void debug(String s) {
    log.log(FQCN, Level.DEBUG, s, null);
  }

  @Override
  public void debug(String format, Object arg) {
    log.log(FQCN, Level.DEBUG, format, (Throwable) arg);
  }

  @Override
  public void debug(String format, Object arg1, Object arg2) {
    log.log(FQCN, Level.DEBUG, format, (Throwable) arg2);
  }

  @Override
  public void trace(String s) {
    log.log(FQCN, Level.TRACE, s, null);
  }

  @Override
  public void warn(String s) {
    log.log(FQCN, Level.WARN, s, null);
  }

  @Override
  public void warn(String s, Object arg) {
    log.log(FQCN, Level.WARN, s, (Throwable) arg);
  }

  @Override
  public void warn(String s, Object arg1, Object arg2) {
    log.log(FQCN, Level.WARN, s, (Throwable) arg2);
  }

}
