package com.changhong.sei.serial.service;

import com.changhong.sei.core.dao.BaseEntityDao;
import com.changhong.sei.core.service.BaseEntityService;
import com.changhong.sei.serial.dao.BarCodeAssociateDao;
import com.changhong.sei.serial.entity.BarCodeAssociate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * <strong>实现功能:</strong>
 * <p>编号生成器配置服务逻辑实现</p>
 *
 * @author 刘松林
 */
@Service
public class BarCodeAssociateService extends BaseEntityService<BarCodeAssociate> {
    private final Logger log = LoggerFactory.getLogger(BarCodeAssociateService.class);

    @Autowired
    private BarCodeAssociateDao barCodeAssociateDao;

    @Override
    protected BaseEntityDao<BarCodeAssociate> getDao() {
        return barCodeAssociateDao;
    }

}
