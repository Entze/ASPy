from dataclasses import dataclass, field
from typing import Sequence

from aspy.ClauseElement import ClauseElement
from aspy.Directive import Directive
from aspy.Rule import Rule


@dataclass(order=True, frozen=True)
class Goal(Rule):
    body: Sequence[ClauseElement] = field(default_factory=tuple)
    head: Directive = field(default=Directive.true(), init=False)

    @property
    def head_signature(self) -> str:
        return "#true/0."

    def __str__(self):
        if self.body:
            return '#true :- {}.'.format(Rule.fmt_body(self.body))
        else:
            return '#true.'

    def variable_normal_form(self):
        return self
