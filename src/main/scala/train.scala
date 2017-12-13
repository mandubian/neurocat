package neurocat


// TODO Generic trainer
// trait Train[Params, In, Out] {

//   // hard coded for Mat for now
//   def train(
//     learn: Learn.Aux[Params, In, Out]
//   )(
//     initParams: P
//   , trainDataIn: Mat[S, NbSamples x InRows]
//   , trainDataOut: Mat[S, NbSamples x OutRows]
//   )(
//     implicit nbSamples: SafeInt[NbSamples]
//     , showInRow: Show[Mat[S, InRows x 1]]
//     , showOutRows: Show[Mat[S, OutRows x 1]]
//     , showP: Show[P]
//   ): P = {
//     var params: P = initParams
//     println(s"params0:${showP.show(params)}")
//     (0 until 10).foreach { _ =>
//       (0 until nbSamples).foreach { i =>
//         val inRow = new Mat[S, InRows x 1](trainDataIn.value.getRow(i).transpose())
//         val outRow = new Mat[S, OutRows x 1](trainDataOut.value.getRow(i).transpose())
//         // println(s"params:${showP.show(params)} inRow:${showInRow.show(inRow)} outRow: ${showOutRows.show(outRow)}")
//         params = learn.update(params)(inRow)(outRow)
//       }
//     }

//     params
//   }  
// }