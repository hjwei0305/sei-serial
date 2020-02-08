package com.changhong.sei.serial.dao;

import com.changhong.sei.core.dao.BaseEntityDao;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * <strong>实现功能:</strong>
 * <p>编号生成器配置数据访问接口</p>
 *
 * @author 王锦光 wangj
 * @version 1.0.1 2017-10-20 9:41
 */
@Repository
public interface SerialNumberConfigDao extends BaseEntityDao<SerialNumberConfig> {

    SerialNumberConfig findByEntityClassNameAndIsolationCode(String name, String className);

    @Modifying
    @Transactional
    @Query("update SerialNumberConfig snc set snc.currentSerial = :currentNumber where id = :id")
    void updateCurrentSerial(@Param("id") String id, @Param("currentNumber") long currentNumber);
}
