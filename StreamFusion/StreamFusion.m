BeginPackage["StreamFusion`"]

ClearAll[
	SSkip,
	Stream,
	Done,
	Yield,

	SMap,
	SSelect,
	SFoldR,
	SFoldL,
	SJoin,
	SZip
];

Begin["`Private`"]

Stream[xs_List] := Module[{next},
	next[{}] = Done;
	next[{h_, t___}] := Yield[h, {t}];
	Stream[next, xs]
]

Stream[UnStream[s_]] := (Print["Removed step"]; s)

UnStream /: Format[UnStream[s_], StandardForm] := s

Stream /: Format[Stream[next0_, s0_], StandardForm] := Module[{unfold},
	unfold[s_] := Module[{iunfold},
		iunfold[Done] = {};
		iunfold[SSkip[sp_]] := unfold[sp];
		iunfold[y:Yield[x_, sp_]] := Prepend[unfold[sp], x];
		iunfold[next0[s]]
	];
	unfold[s0]
]



SMap[f_, l_] := UnStream[Map[f, Stream[l]]]
SSelect[l_, f_] := UnStream[Select[Stream[l], f]]
SFoldR[f_, z_, l_] := UnStream[SFoldR[f, z, Stream[l]]]
SFoldL[f_, z_, l_] := UnStream[SFoldL[f, z, Stream[l]]]
SJoin[l0_, l1_] := UnStream[Join[Stream[l0], Stream[l1]]]
SZip[l0_, l1_] := UnStream[SZip[Stream[l0], Stream[l1]]]

Stream /: Map[f_, Stream[next0_, s0_]] := Module[{next},
	next[s_] := Module[{inext},
		inext[Done] = Done;
		inext[SSkip[sp_]] := SSkip[sp];
		inext[Yield[x_, sp_]] := Yield[f[x], sp];
		inext[next0[s]]
	];
	Stream[next, s0]
]

Stream /: Select[Stream[next0_, s0_], f_] := Module[{next},
	next[s_] := Module[{inext},
		inext[Done] = Done;
		inext[SSkip[sp_]] := SSkip[sp];
		inext[Yield[x_, sp_]] := If[TrueQ[f[x]],
			Yield[x, sp],
			SSkip[sp]
		];
		inext[next0[s]]
	];
	Stream[next, s0]
]

Stream /: SFoldR[f_, z_, Stream[next_, s0_]] := Module[{go},
	go[s_] := Module[{igo},
		igo[Done] = z;
		igo[SSkip[sp_]] := go[sp];
		igo[Yield[x_, sp_]] := f[x, go[sp]];
		go[next[s]]
	];
	go[s0]
]

Stream /: SFoldL[f_, z0_, Stream[next_, s0_]] := Module[{go},
	go[z_, s_] := Module[{igo},
		igo[Done] = z;
		igo[SSkip[sp_]] := go[z, sp];
		igo[Yield[x_, sp_]] := go[f[z, x], sp];
		go[next[s]]
	];
	go[z0, s0]
]

Stream /: Join[Stream[nexta_, sa0_], Stream[nextb_, sb0_]] :=
	Module[{next, left, right},
		next[left[sa_]] := Module[{inext},
			inext[Done] = SSkip[right[sb0]];
			inext[SSkip[sap_]] := SSkip[left[sap]];
			inext[Yield[x_, sap_]] := Yield[x, left[sap]];
			inext[nexta[sa]]
		];
		next[right[sb_]] := Module[{inext},
			inext[Done] = Done;
			inext[SSkip[sbp_]] := SSkip[right[sbp]];
			inext[Yield[x_, sbp_]] := Yield[x, right[sbp]];
			inext[nextb[sb]]
		];
		Stream[next, left[sa0]]
	]

Stream /: SZip[Stream[nexta_, sa0_], Stream[nextb_, sb0_]] :=
	Module[{next, left, right, nothing, just},
		next[{sa_, sb_, nothing}], sb_, nothing] := Module[{inext},
			inext[Done] = SSkip[right[sb0]];
			inext[SSkip[sap_]] := SSkip[{sap, sb, nothing]};
			inext[Yield[x_, sap_]] := SSkip[{sap, sb, just[a]}];
			inext[nexta[sa]]
		];
		next[{sap_, sb_, just[a_]}] := Module[{inext},
			inext[Done] = Done;
			inext[SSkip[sbp_]] := SSkip[{sap, sbp, just[a]}];
			inext[Yield[x_, sbp_]] := Yield[{a, x}, {sap, sbp, nothing}];
			inext[nextb[sb]]
		];
		Stream[next, {sa0, sb0, nothing}]
	]

End[]

EndPackage[]

