import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Acceptance Sampling")
	description	: qsTr("Sampling for acceptance")
	icon		: "acceptance_sampling.svg"
	requiresData: false
	hasWrappers:  true
	preloadData:  false
	

	GroupTitle
	{
		title:	qsTr("Attribute Sampling")
		icon:	"attribute.svg"
	}

	Analysis
	{
		title:	qsTr("Create Attribute Plan")
		qml:	"CreateAttributePlan.qml"
		func:	"CreateAttributePlan"
	}

	Analysis
	{
		title:	qsTr("Analyze Attribute Plan")
		qml:	"AnalyzeAttributePlan.qml"
		func:	"AnalyzeAttributePlan"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Variable Sampling")
		icon:	"variable.svg"
	}

	Analysis
	{
		title:	qsTr("Create Variable Plan")
		qml:	"CreateVariablePlan.qml"
		func:	"CreateVariablePlan"
	}

	Analysis
	{
		title:	qsTr("Analyze Variable Plan")
		qml:	"AnalyzeVariablePlan.qml"
		func:	"AnalyzeVariablePlan"
	}

	Analysis
	{
		menu:	qsTr("Accept/Reject Lots")
		title:	qsTr("Accept/Reject Lots (k-method)")
		qml:	"DecideVariableLots.qml"
		func:	"DecideVariableLots"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Bayesian Sampling")
		icon:	"acceptance_sampling.svg"
	}

	Analysis
	{
		title:	qsTr("Bayesian Sampling")
		qml:	"BayesianSampling.qml"
		func:	"BayesianSampling"
		preloadData: true
	}

	Analysis
	{
		title:	qsTr("Bayesian State-Space Sampling")
		qml:	"BayesianSSMforAS.qml"
		func:	"BayesianSSMforAS"
		preloadData: true
	}
}
