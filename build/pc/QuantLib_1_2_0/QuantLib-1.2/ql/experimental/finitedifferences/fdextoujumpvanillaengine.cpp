/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2011 Klaus Spanderen

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file fdoujumpvanillaengine.cpp
    \brief Finite Differences Ornstein Uhlenbeck plus exponential jumps engine 
           for simple swing options
*/

#include <ql/exercise.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/experimental/processes/extouwithjumpsprocess.hpp>
#include <ql/experimental/processes/extendedornsteinuhlenbeckprocess.hpp>
#include <ql/methods/finitedifferences/operators/fdmlinearoplayout.hpp>
#include <ql/methods/finitedifferences/meshers/fdmmeshercomposite.hpp>
#include <ql/experimental/finitedifferences/fdmextoujumpmodelinnervalue.hpp>
#include <ql/methods/finitedifferences/stepconditions/fdmamericanstepcondition.hpp>
#include <ql/methods/finitedifferences/stepconditions/fdmbermudanstepcondition.hpp>
#include <ql/methods/finitedifferences/stepconditions/fdmstepconditioncomposite.hpp>
#include <ql/methods/finitedifferences/meshers/exponentialjump1dmesher.hpp>
#include <ql/experimental/finitedifferences/fdmextoujumpsolver.hpp>
#include <ql/methods/finitedifferences/meshers/fdmsimpleprocess1dmesher.hpp>
#include <ql/experimental/finitedifferences/fdextoujumpvanillaengine.hpp>

namespace QuantLib {

    FdExtOUJumpVanillaEngine::FdExtOUJumpVanillaEngine(
                      const boost::shared_ptr<ExtOUWithJumpsProcess>& process,
                      const boost::shared_ptr<YieldTermStructure>& rTS,
                      Size tGrid, Size xGrid, Size yGrid,
                      const FdmSchemeDesc& schemeDesc)
    : process_(process),
      rTS_(rTS),
      tGrid_(tGrid),
      xGrid_(xGrid),
      yGrid_(yGrid),
      schemeDesc_(schemeDesc) {
    }
                      
    void FdExtOUJumpVanillaEngine::calculate() const {
        // 1. Layout

        std::vector<Size> dim;
        dim.push_back(xGrid_);
        dim.push_back(yGrid_);
        const boost::shared_ptr<FdmLinearOpLayout> layout(
                                            new FdmLinearOpLayout(dim));

        // 2. Mesher
        const Time maturity 
            = rTS_->dayCounter().yearFraction(rTS_->referenceDate(),
                                              arguments_.exercise->lastDate());
        const boost::shared_ptr<StochasticProcess1D> ouProcess(
                              process_->getExtendedOrnsteinUhlenbeckProcess());
        const boost::shared_ptr<Fdm1dMesher> xMesher(
                     new FdmSimpleProcess1dMesher(xGrid_, ouProcess,maturity));

        const boost::shared_ptr<Fdm1dMesher> yMesher(
            new ExponentialJump1dMesher(yGrid_, 
                                        process_->beta(), 
                                        process_->jumpIntensity(),
                                        process_->eta()));

        std::vector<boost::shared_ptr<Fdm1dMesher> > meshers;
        meshers.push_back(xMesher);
        meshers.push_back(yMesher);
        const boost::shared_ptr<FdmMesher> mesher (
                                   new FdmMesherComposite(layout, meshers));

        // 3. Calculator
        const boost::shared_ptr<FdmInnerValueCalculator> calculator(
                    new FdmExtOUJumpModelInnerValue(arguments_.payoff, mesher));

        // 4. Step conditions
        const boost::shared_ptr<FdmStepConditionComposite> conditions =
            FdmStepConditionComposite::vanillaComposite(
                                DividendSchedule(), arguments_.exercise, 
                                mesher, calculator, 
                                rTS_->referenceDate(), rTS_->dayCounter());

        // 5. Boundary conditions
        const std::vector<boost::shared_ptr<FdmDirichletBoundary> > boundaries;
        
        // 6. set-up solver
        FdmSolverDesc solverDesc = { mesher, boundaries, conditions,
                                    calculator, maturity, tGrid_, 0 };

        const boost::shared_ptr<FdmExtOUJumpSolver> solver(
            new FdmExtOUJumpSolver(Handle<ExtOUWithJumpsProcess>(process_), 
                                   rTS_, solverDesc, schemeDesc_));
      
        const Real x = process_->initialValues()[0];
        const Real y = process_->initialValues()[1];
        results_.value = solver->valueAt(x, y);      
    }
}
