package org.validoc.play

import javax.inject.Inject

import play.api.mvc.{BaseController, ControllerComponents}

@Singleton
class PlayController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  Action.async { implicit request =>

    ??? 
  }
}
